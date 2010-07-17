%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer.core.user).

-export([create/1, create/2, delete/2, exists/1, exists/2, is_valid_id/1]).

-import(httpc).
-import(lists).
-import(mnesia).
-import(string).
-import(qlc).

-import(jsonerl).
-import(log4erl).
-import(uuid).

-import(platformer.core.liaison).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jsonerl.hrl").
-include_lib("platformer.hrl").

%% @doc Check whether there is a local record for a user with the given id.
%%  No attempt is made to check remove servers (For that, use {@link exists/2}.)
%%
%% @spec exists(string()) -> {bool(), active | deleted}
exists(Id) ->
    log4erl:debug("Checking for local record of user ~s", [Id]),
    Result = db:find(qlc:q([X || X <- mnesia:table(pfuser), X#pfuser.id == Id])),
    case length(Result) of
        1 ->
            log4erl:debug("Record for user ~s found in database.", [Id]),
            User = hd(Result),
            case User#pfuser.status of
                active ->
                    log4erl:debug("User ~s is active.", [Id]),
                    {true, active};
                deleted ->
                    log4erl:debug("User ~s was previously deleted.", [Id]),
                    {false, deleted}
            end;
        0 ->
            log4erl:debug("User ~s does not exist locally.", [Id]),
            {false, unknown};
        _ ->
            throw({codingError, "Multiple results found for user ~s!", [Id]})
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
    
exists(Id, Nodes, #envelope{} = Envelope) when is_binary(Id) ->
    log4erl:debug("Checking other nodes for existence of user ~s", [Id]),
    exists(binary_to_list(Id), Nodes, Envelope);

%% @spec exists(string(), [NodeRecord]) -> {bool(), active | deleted}
exists(Id, [#pfnode{} = Node|Nodes], #envelope{} = Envelope) ->
    case node:is_me(Node) of
        false ->
            Address = node:get_address(Node),
            log4erl:debug("Checking node at ~s for user ~s.", [Address, Id]),
            case httpc:request(head, {lists:concat([Address, "/user/", Id]),
                                      [{"X-Platformer-Memo-Source", node:my_address()}]}, [], []) of
                {ok, {{_, Status, _}, _, _}} ->
                    case Status of
                        200 ->
                            log4erl:debug("User ~s found at node ~s.", [Id, Address]),
                            {true, active};
                        410 ->
                            log4erl:debug("User ~s was deleted according to node ~s.", [Id, Address]),
                            {false, deleted};
                        _ ->
                            log4erl:debug("Node ~s returns status ~B for user ~s; checking other nodes.", [Address, Status, Id]),
                            exists(Id, Nodes, Envelope)
                    end;
                {error, Reason} ->
                    log4erl:debug("An error occurred when checking node ~s for user ~s: ~p", [Address, Id, Reason]),
                    exists(Id, Nodes, Envelope)
            end;
        true -> exists(Id, Nodes, Envelope)
    end;

exists(_Id, [], _) -> {false, unknown}.

%% @doc Create a brand new user.
%%
%% @spec create() -> {string(), string()}
create(#envelope{} = Envelope) ->
    log4erl:debug("Creating new user."),
    IdString = string:concat("platformer_user_", util:uuid()),
    create(IdString, Envelope).

%% @doc Create a local record of a user that already exists somewhere else.
%%
%% @spec create(string(), {string(), integer(), string()}) -> {string(), string()}
create(IdString, #envelope{priority=Priority, source=Source} = Envelope) ->
    Id = list_to_binary(IdString),
    User = #pfuser{id=Id, status=active, last_modified=util:now_int(), source=Source},
    case db:write(User) of
        {atomic, ok} ->
            log4erl:debug("Created new user ~s with priority ~B.", [IdString, Priority]),
            spawn_link(platformer.core.liaison, propagate, [put, "user", IdString, Envelope]),
            {Id, string:concat("/user/", IdString)};
        {aborted, Error} ->
            log4erl:debug("Error in creating user ~s: ~p", [IdString, Error]),
            throw(Error)
    end.

delete(IdString, #envelope{} = Envelope) ->
    Id = list_to_binary(IdString),
    F = fun() ->
                [User] = mnesia:read(pfuser, Id, write),
                mnesia:write(User#pfuser{status=deleted, last_modified=util:now_int()})
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            log4erl:debug("Deleted user ~s locally.", [IdString]);
        {aborted, _Error} ->
            log4erl:debug("Could not delete user ~s locally.", [IdString])
    end,
    log4erl:debug("Propagating deletion message."),
    spawn_link(platformer.core.liaison, propagate, [delete, "user", IdString, Envelope]).

is_valid_id(Id) ->
    string:left(Id, 16) =:= "platformer_user_"
        andalso
        util:is_valid_uuid(string:substr(Id, 17)).
