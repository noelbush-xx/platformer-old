%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%%
%% @doc Communication among nodes in Platformer is carried out by
%% propagating <em>memos</em>.  There are specialized memos
%% corresponding to each of the different sorts of entities that
%% Platformer is concerned with: users, positions, other nodes, etc.
%% This module provides some generic services common to all memo
%% types, and specifies (using Erlang's behaviour mechanism) the
%% methods that any "concrete subtype" of memo must implement.

-module(platformer_memo).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("platformer.hrl").

-compile({parse_transform, exprecs}).
-export_records([platformer_user, platformer_node]).

-export([create/2, create/4, delete/3, exists/2, exists/3, get/2]).
-export([check_token/1, is_valid_token/2, is_valid_priority/2, propagate/4, propagate/5, propagate/6, propagate/7]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{create, 1},        % Create a brand new memo item, using a given envelope.
     {create, 2},        % Create a record for an existing memo item, given a specification and an envelope.
     {delete, 2},        % Mark a record for a memo item as deleted.
     {exists, 1},        % Check whether a record for a memo item exists.
     {exists, 2},        % As above, but including an envelope.
     {get, 1},           % Retrieve a memo by id.
     {is_valid_id, 1},   % Check whether a given string is a valid id for the memo type.
     {to_json, 1}        % Return a JSON representation of a memo.
    ];
behaviour_info(_Other) -> undefined.


%% @doc Create a brand new memo of a given type.  Return an id and a
%% path for the new memo.
%%
%% @spec create(atom(), string(), envelope()) -> {Id::string(), Path::string()}
create(Type, #envelope{} = Envelope) ->
    log4erl:debug("Creating new ~s.", [Type]),
    Id = lists:concat(["platformer_", Type, "_", platformer_util:uuid()]),
    apply(memo_module(Type), create, [Id, Envelope]).

%% @doc Create a local record of a memo that already exists somewhere else.
%%
%% @spec create(string(), string(), record(), envelope()) -> {Id::string(), Path::string()}
create(Type, Id, Record, #envelope{priority=Priority} = Envelope) ->
    case platformer_db:write(Record) of
        {atomic, ok} ->
            log4erl:debug("Created new ~s ~s with priority ~B.", [Type, Id, Priority]),
            Json = apply(memo_module(Type), to_json, [Id]),
            spawn_link(platformer_memo, propagate, [put, Type, Id, Json, Envelope]),
            {Id, lists:concat(["/", Type, "/", Id])};
        {aborted, Error} ->
            log4erl:debug("Error in creating ~s ~s: ~p", [Type, Id, Error]),
            throw(Error)
    end.


%% @doc Mark a local record of a memo as deleted.
%%
%% @spec delete(string(), string(), envelope()) -> none()
delete(Type, Id, #envelope{} = Envelope) ->
    log4erl:debug("Delete ~s ~p.", [Type, Id]),
    case apply(memo_module(Type), get, [list_to_binary(Id)]) of
        not_found -> log4erl:debug("~s ~s not found locally; could not delete.", [Type, Id]);
        Record ->
            RecordSource = platformer_memo:'#get-'(source, Record),
            F = fun() ->
                        mnesia:write(platformer_memo:'#from-list'([{status, deleted},
                                                                   {last_modified, platformer_util:now_int()}], Record))
                end,
            case mnesia:transaction(F) of
                {atomic, ok} ->
                    log4erl:debug("Deleted ~s ~s locally.", [Type, Id]);
                {aborted, _Error} ->
                    log4erl:debug("Could not delete ~s ~s locally.", [Type, Id])
            end,
            log4erl:debug("Propagating deletion memo."),
            
            SourceNode = platformer_node:get(platformer_node:get_id((RecordSource))),
            spawn(platformer_memo, propagate, [delete, Type, Id, Envelope,
                                                    case platformer_node:is_me(SourceNode) of
                                                        true -> [];
                                                        false -> [SourceNode]
                                                    end,
                                               []])
    end.

%% @doc Get a memo by id.
%%
%% @spec get(string(), string()) -> platformer_memo()
get(Type, Id) ->
    RecordType = memo_module(Type),
    Result = platformer_db:find(qlc:q([X || X <- mnesia:table(RecordType),
                                            platformer_memo:'#get-'(id, X) == Id])),
    case length(Result) of
        1 ->
            log4erl:debug("Record for ~s ~s found in database.", [Type, Id]),
            hd(Result);
        0 ->
            log4erl:debug("~s ~s does not exist locally.", [Type, Id]),
            not_found;
        _ ->
            throw({codingError, "Multiple results found for ~s ~s!", [Type, Id]})
    end.

%% @doc Check whether there is a local record for a memo with the given id.
%%  No attempt is made to check remove servers (For that, use {@link exists/2}.)
%%
%% @spec exists(string(), string()) -> {bool(), active | deleted}
exists(Type, Id) ->
    log4erl:debug("Checking for local record of ~s ~s", [Type, Id]),
    case apply(memo_module(Type), get, [Id]) of
        not_found -> {false, unknown};
        Record ->
            case platformer_memo:'#get-'(status, Record) of
                active ->
                    log4erl:debug("~s ~s is active.", [Type, Id]),
                    {true, active};
                deleted ->
                    log4erl:debug("~s ~s was previously deleted.", [Type, Id]),
                    {false, deleted}
            end
    end.

%% Check whether the indicated memo exists on this or any known node.
%%
%% @spec exists(string(), string(), envelope()) -> {bool(), active | deleted}
exists(Type, Id, Envelope) ->
    Module = memo_module(Type),
    %% First check the local database.
    case apply(Module, exists, [Id]) of
        {true, Status} -> {true, Status};
        {false, deleted} -> {false, deleted};
        {false, unknown} ->
            exists(Type, Id, platformer_node:get_list(), Envelope)
    end.

%% @spec exists(binary(), [platformer_node()], envelope()) -> {bool(), active | deleted}
exists(Type, Id, Nodes, #envelope{} = Envelope) when is_binary(Id) ->
    log4erl:debug("Checking up to ~B other nodes for existence of ~s ~s", [length(Nodes), Type, Id]),
    exists(Type, binary_to_list(Id), Nodes, Envelope);

exists(Type, Id, [#platformer_node{} = Node|Nodes], #envelope{token=Token, priority=Priority} = Envelope) ->
    case platformer_node:is_me(Node) of
        false ->
            Address = platformer_node:get_address(Node),
            log4erl:debug("Checking node at ~s for ~s ~s.", [Address, Type, Id]),
            case httpc:request(head, {lists:concat([Address, "/", Type, "/", Id]),
                                      [{"X-Platformer-Memo-Token", Token},
                                       {"X-Platformer-Memo-Priority", integer_to_list(Priority)},
                                       {"X-Platformer-Memo-Source", platformer_node:my_address()}]},
                               platformer_util:httpc_standard_http_options(), platformer_util:httpc_standard_options()) of
                {ok, {{_, Status, _}, _, _}} ->
                    case Status of
                        200 ->
                            log4erl:debug("~s ~s found at node ~s.", [Type, Id, Address]),
                            % Add this memo to local db (without propagating)
                            apply(memo_module(Type), create, [Type, Id, Envelope#envelope{priority=0, source=Address}]),
                            {true, active};
                        410 ->
                            log4erl:debug("~s ~s was deleted according to node ~s.", [Type, Id, Address]),
                            %% TODO: Add a deleted record to our db (?)
                            {false, deleted};
                        _ ->
                            log4erl:debug("Node ~s returns status ~B for ~s ~s; checking other nodes.", [Address, Status, Type, Id]),
                            exists(Type, Id, Nodes, Envelope)
                    end;
                {error, timeout} ->
                    log4erl:debug("Request timed out when checking node ~s for  ~s.", [Address, Type, Id]),
                    exists(Type, Id, Nodes, Envelope);
                Result ->
                    log4erl:debug("An unexpected result was obtained when checking node ~s for ~s ~s: ~p", [Address, Type, Id, Result]),
                    exists(Type, Id, Nodes, Envelope)
            end;
        true ->
            log4erl:debug("Skipping check of self."),
            exists(Type, Id, Nodes, Envelope)
    end;

exists(_Type, _Id, [], _) ->
    log4erl:debug("No more nodes to check."),
    {false, unknown}.


%% @doc For internal use: return an atom naming the module that should
%% handle the given type (named as a string).  The same atom should
%% correspond to the record type used to store the type.
%%
%% @spec memo_module(string()) -> atom()
memo_module(Type) ->
    list_to_atom(string:concat("platformer_", Type)).

is_valid_token(S, Description) ->
    case platformer_util:is_valid_uuid(S) of
        true -> {true, S};
        false -> {false, Description ++ " must be a valid v4 uuid."}
    end.

is_valid_priority(S, Description) ->
    try list_to_integer(S) of
        I ->
            case I < 0 of
                true ->
                    {false, Description ++ " must be positive."};
                false ->
                    MaxPriority = platformer_util:get_param(memo_priority_max),
                    case I > MaxPriority of
                        true ->
                            {false, Description ++ " must be no greater than " ++ integer_to_list(MaxPriority) ++ "."};
                        false ->
                            {true, I}
                    end
            end
    catch
        error:badarg -> {false, Description ++ " must be an integer."}
    end.

%% @doc Check whether the given token has been seen before.  If it has
%% not, return <code>ok</code> and add the token to the
%% <code>platformer_token</code> database table.  If it has, return
%% <code>already_seen</code>.
%%
%% @spec check_token(string()) -> ok | already_seen
check_token(Id) ->
    case length(platformer_db:find(qlc:q([X || X <- mnesia:table(platformer_token), X#platformer_token.id =:= Id]))) of
        0 ->
            Token = #platformer_token{id=Id, received=platformer_util:now_int()},
            case platformer_db:write(Token) of
                {atomic, ok} ->
                    ok;
                {aborted, Error} ->
                    throw(Error)
            end;
        1 ->
            already_seen
    end.

%% @doc Propagate an item with no body.
%%
%% @spec propagate(atom(), string(), string(), envelope(), list(), list()) -> ok
propagate(Action, Type, Id, #envelope{} = Envelope, MandatoryTargets, Omit) when Action =:= put orelse Action =:= delete ->
    propagate(Action, Type, Id, undefined, Envelope, MandatoryTargets, Omit).

%% @doc Convenience function for {@link propagate/5} where there are
%% no mandatory targets and no nodes to omit.
propagate(Action, Type, Id, #envelope{} = Envelope) ->
    propagate(Action, Type, Id, Envelope, [], []).

%% @doc Propagate an item with a body.
%%
%% @spec propagate(atom(), string(), string(), string(), envelope(), list(), list()) -> ok
propagate(Action, Type, Id, _, #envelope{priority=0}, _, _) ->
    log4erl:debug("Not propagating priority 0 ~p of ~s ~s.", [Action, Type, Id]),
    ok;
propagate(Action, Type, Id, Body, #envelope{priority=Priority} = Envelope, MandatoryTargets, Omit) when Action =:= put orelse Action =:= delete ->
    Nodes = lists:append(MandatoryTargets, platformer_node:get_random_list({count, Priority}, [], lists:append(Omit, [(platformer_node:me())#platformer_node.id]))),
    NodeCount = length(Nodes),
    if
        NodeCount > 0 ->
            log4erl:debug("Propagating priority ~B ~p of ~s ~s to ~B node(s).", [Priority - 1, Action, Type, Id, NodeCount]),
            propagate_to_nodes(Nodes, Action, Type, Id, Body, Envelope#envelope{priority=integer_to_list(Priority - 1)});
        NodeCount =:= 0 ->
            log4erl:debug("No nodes to which to propagate priority ~B ~p of ~s ~s.", [Priority - 1, Action, Type, Id]),
            ok
    end.

%% @doc Convenience function for {@link propagate/7} where there are
%% no mandatory targets and no nodes to omit.
propagate(Action, Type, Id, Body, #envelope{} = Envelope) ->
    propagate(Action, Type, Id, Body, Envelope, [], []).

propagate_to_nodes([Node|Rest], Action, Type, Id, Body, #envelope{token=Token, priority=Priority} = Envelope) ->
    case platformer_node:is_me(Node) of
        true -> log4erl:debug("No need to propagate to self.");
        false ->
            Address = platformer_node:get_address(Node),
            log4erl:debug("Propagating ~p of ~s to node ~s.", [Action, Type, Address]),
            Uri = lists:concat([Address, "/", Type, "/", Id]),
            Headers = [{"X-Platformer-Memo-Token", Token},
                       {"X-Platformer-Memo-Priority", Priority},
                       {"X-Platformer-Memo-Source", platformer_node:my_address()}],
            if Body =:= undefined ->
                    if Action =:= put ->
                            throw({error, "httpc requires that a PUT have a body."});
                       Action =/= put ->
                            httpc:request(Action, {Uri, Headers}, platformer_util:httpc_standard_http_options(), platformer_util:httpc_standard_options())
                    end;
               Body =/= undefined ->
                    httpc:request(Action, {Uri, Headers, "text/javascript", Body}, platformer_util:httpc_standard_http_options(), platformer_util:httpc_standard_options())
            end
    end,
    propagate_to_nodes(Rest, Action, Type, Id, Body, Envelope);
propagate_to_nodes([], Action, Type, Id, _, _) ->
    log4erl:debug("No more nodes to which to propagate ~p of ~s ~s.", [Action, Type, Id]),
    ok.
