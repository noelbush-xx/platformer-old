%% @doc Represents a memo about a user of Platformer.
%%
%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer_user).
-behaviour(platformer_memo).

-export([create/1, create/2, delete/2, exists/1, exists/2, get/1, is_valid_id/1, to_json/1]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jsonerl.hrl").
-include_lib("platformer.hrl").

%% @doc Create a brand new user.  Return an id and a path.
%%
%% @spec create(envelope()) -> {Userid::string(), Path::string()}
create(#envelope{} = Envelope) ->
    log4erl:debug("Creating new user."),
    IdString = string:concat("platformer_user_", util:uuid()),
    create(IdString, Envelope).

%% @doc Create a local record of a user that already exists somewhere else.
%%
%% @spec create(Id::string(), envelope()) -> {Userid::string(), Path::string()}
create(IdString, #envelope{priority=Priority, source=Source} = Envelope) ->
    Id = list_to_binary(IdString),
    User = #pfuser{id=Id, status=active, last_modified=util:now_int(), source=Source},
    case db:write(User) of
        {atomic, ok} ->
            log4erl:debug("Created new user ~s with priority ~B.", [IdString, Priority]),
            spawn_link(platformer.core.memo, propagate, [put, "user", IdString, user:to_json(IdString), Envelope]),
            {Id, string:concat("/user/", IdString)};
        {aborted, Error} ->
            log4erl:debug("Error in creating user ~s: ~p", [IdString, Error]),
            throw(Error)
    end.

%% @doc Mark a local record of a user as deleted.
%%
%% @spec delete(Id::string(), envelope()) -> none()
delete(IdString, #envelope{} = Envelope) ->
    log4erl:debug("Delete user ~p.", [IdString]),
    Id = list_to_binary(IdString),
    case user:get(Id) of
        not_found -> log4erl:debug("User ~s not found locally; could not delete.", [IdString]);
        User ->
            UserSource = User#pfuser.source,
            F = fun() ->
                        mnesia:write(User#pfuser{status=deleted, last_modified=util:now_int()})
                end,
            case mnesia:transaction(F) of
                {atomic, ok} ->
                    log4erl:debug("Deleted user ~s locally.", [IdString]);
                {aborted, _Error} ->
                    log4erl:debug("Could not delete user ~s locally.", [IdString])
            end,
            log4erl:debug("Propagating deletion memo."),
            
            Source = node:get(node:get_id((UserSource))),
            spawn(platformer.core.memo, propagate, [delete, "user", IdString, Envelope,
                                                    case node:is_me(Source) of true -> []; false -> [Source] end,
                                                    []])
    end.

%% @doc Get a user by id.
%%
%% @spec get(string()) -> pfuser()
get(Id) ->
    Result = db:find(qlc:q([X || X <- mnesia:table(pfuser), X#pfuser.id == Id])),
    case length(Result) of
        1 ->
            log4erl:debug("Record for user ~s found in database.", [Id]),
            hd(Result);
        0 ->
            log4erl:debug("User ~s does not exist locally.", [Id]),
            not_found;
        _ ->
            throw({codingError, "Multiple results found for user ~s!", [Id]})
    end.

%% @doc Check whether there is a local record for a user with the given id.
%%  No attempt is made to check remove servers (For that, use {@link exists/2}.)
%%
%% @spec exists(string()) -> {bool(), active | deleted}
exists(Id) ->
    log4erl:debug("Checking for local record of user ~s", [Id]),
    case user:get(Id) of
        not_found -> {false, unknown};
        User ->
            case User#pfuser.status of
                active ->
                    log4erl:debug("User ~s is active.", [Id]),
                    {true, active};
                deleted ->
                    log4erl:debug("User ~s was previously deleted.", [Id]),
                    {false, deleted}
            end
    end.
    
%% @spec exists(binary(), envelope()) -> {bool(), active | deleted}
exists(Id, #envelope{} = Envelope) ->
    %% First check the local database.
    case exists(Id) of
        {true, Status} -> {true, Status};
        {false, deleted} -> {false, deleted};
        {false, unknown} ->
            exists(Id, node:get_list(), Envelope)
    end.
    
%% @spec exists(binary(), [pfnode()], envelope()) -> {bool(), active | deleted}
exists(Id, Nodes, #envelope{} = Envelope) when is_binary(Id) ->
    log4erl:debug("Checking up to ~B other nodes for existence of user ~s", [length(Nodes), Id]),
    exists(binary_to_list(Id), Nodes, Envelope);

exists(Id, [#pfnode{} = Node|Nodes], #envelope{token=Token, priority=Priority} = Envelope) ->
    case node:is_me(Node) of
        false ->
            Address = node:get_address(Node),
            log4erl:debug("Checking node at ~s for user ~s.", [Address, Id]),
            case httpc:request(head, {lists:concat([Address, "/user/", Id]),
                                      [{"X-Platformer-Memo-Token", Token},
                                       {"X-Platformer-Memo-Priority", integer_to_list(Priority)},
                                       {"X-Platformer-Memo-Source", node:my_address()}]},
                               util:httpc_standard_http_options(), util:httpc_standard_options()) of
                {ok, {{_, Status, _}, _, _}} ->
                    case Status of
                        200 ->
                            log4erl:debug("User ~s found at node ~s.", [Id, Address]),
                            % Add this user to local db (without propagating)
                            create(Id, Envelope#envelope{priority=0, source=Address}),
                            {true, active};
                        410 ->
                            log4erl:debug("User ~s was deleted according to node ~s.", [Id, Address]),
                            %% TODO: Add a deleted record to our db (?)
                            {false, deleted};
                        _ ->
                            log4erl:debug("Node ~s returns status ~B for user ~s; checking other nodes.", [Address, Status, Id]),
                            exists(Id, Nodes, Envelope)
                    end;
                {error, timeout} ->
                    log4erl:debug("Request timed out when checking node ~s for user ~s.", [Address, Id]),
                    exists(Id, Nodes, Envelope);
                Result ->
                    log4erl:debug("An unexpected result was obtained when checking node ~s for user ~s: ~p", [Address, Id, Result]),
                    exists(Id, Nodes, Envelope)
            end;
        true ->
            log4erl:debug("Skipping check of self."),
            exists(Id, Nodes, Envelope)
    end;

exists(_Id, [], _) ->
    log4erl:debug("No more nodes to check."),
    {false, unknown}.

%% @doc Is the given id valid for a user?
%%
%% @spec is_valid_id(string()) -> bool()
is_valid_id(Id) ->
    string:left(Id, 16) =:= "platformer_user_"
        andalso
        util:is_valid_uuid(string:substr(Id, 17)).

%% @doc Produce a json representation of the user with the given id.
%%
%% @spec to_json(string()) -> string()
to_json(Id) ->
    jsonerl:encode({{user, {{id, Id}}}}).
