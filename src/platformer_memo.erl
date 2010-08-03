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

is_valid_token(S, Description) ->
    case util:is_valid_uuid(S) of
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
                    MaxPriority = util:get_param(memo_priority_max),
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
%% <code>pftoken</code> database table.  If it has, return
%% <code>already_seen</code>.
%%
%% @spec check_token(string()) -> ok | already_seen
check_token(Id) ->
    case length(db:find(qlc:q([X || X <- mnesia:table(pftoken), X#pftoken.id =:= Id]))) of
        0 ->
            Token = #pftoken{id=Id, received=util:now_int()},
            case db:write(Token) of
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
    Nodes = lists:append(MandatoryTargets, node:get_random_list({count, Priority}, [], lists:append(Omit, [(node:me())#pfnode.id]))),
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
    case node:is_me(Node) of
        true -> log4erl:debug("No need to propagate to self.");
        false ->
            Address = node:get_address(Node),
            log4erl:debug("Propagating ~p of ~s to node ~s.", [Action, Type, Address]),
            Uri = lists:concat([Address, "/", Type, "/", Id]),
            Headers = [{"X-Platformer-Memo-Token", Token},
                       {"X-Platformer-Memo-Priority", Priority},
                       {"X-Platformer-Memo-Source", node:my_address()}],
            if Body =:= undefined ->
                    if Action =:= put ->
                            throw({error, "httpc requires that a PUT have a body."});
                       Action =/= put ->
                            httpc:request(Action, {Uri, Headers}, util:httpc_standard_http_options(), util:httpc_standard_options())
                    end;
               Body =/= undefined ->
                    httpc:request(Action, {Uri, Headers, "text/javascript", Body}, util:httpc_standard_http_options(), util:httpc_standard_options())
            end
    end,
    propagate_to_nodes(Rest, Action, Type, Id, Body, Envelope);
propagate_to_nodes([], Action, Type, Id, _, _) ->
    log4erl:debug("No more nodes to which to propagate ~p of ~s ~s.", [Action, Type, Id]),
    ok.
