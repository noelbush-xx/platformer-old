%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer.core.user).

-export([create/0, create/2, delete/1, exists/1, is_valid_id/1]).

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

%% @spec exists({binary(), bool()}) -> {bool(), active | deleted}
exists({Id, CheckOtherNodes}) ->
    %% First check the local database.
    Result = db:find(qlc:q([X || X <- mnesia:table(pfuser),
                                            X#pfuser.id == Id])),
    case length(Result) of
        1 ->
            User = hd(Result),
            case User#pfuser.status of
                active -> {true, active};
                deleted -> {false, deleted}
            end;
        0 ->
            %% Not found locally -- only check other nodes if instructed to do so.
            case CheckOtherNodes of
                true -> exists(Id, node:get_list());
                false -> {false, unknown}
            end;
        _ ->
            throw({codingError, "Multiple results found for id!"})
    end.
    
exists(Id, Nodes) when is_binary(Id) ->
    exists(binary_to_list(Id), Nodes);

%% @spec exists(string(), [NodeRecord]) -> {bool(), active | deleted}
exists(Id, [#pfnode{} = Node|Nodes]) ->
    case node:is_me(Node) of
        false ->
            case httpc:request(head, {lists:concat([node:get_address(Node), "/user/", Id]),
                                      [{"X-Platformer-Message-Source", node:my_address()}]}, [], []) of
                {ok, {{_, Status, _}, _, _}} ->
                    case Status of
                        200 -> {true, active};
                        410 -> {false, deleted};
                        _ -> exists(Id, Nodes)
                    end;
                {error, _Reason} -> exists(Id, Nodes)
            end;
        true -> exists(Id, Nodes)
    end;

exists(_Id, []) -> {false, unknown}.

%% @doc Create a brand new user.
%%
%% @spec create() -> {string(), string()}
create() ->
    log4erl:debug("Creating new user with new propagation envelope."),
    IdString = string:concat("platformer_user_", util:uuid()),
    create(IdString, {util:uuid(), util:get_param(propagation_priority, 3), node:my_address()}).

%% @doc Create a local record of a user that already exists somewhere else.
%%
%% @spec create(string(), {string(), integer(), string()}) -> {string(), string()}
create(IdString, {_Token, Priority, Source} = Envelope) ->
    Id = list_to_binary(IdString),
    User = #pfuser{id=Id, status=active, last_modified=util:now_int(), source=Source},
    case db:write(User) of
        {atomic, ok} ->
            log4erl:debug("Created new user ~s with priority ~B.", [IdString, Priority]),
            spawn_link(platformer.core.liaison, propagate, ["user", IdString, Envelope]),
            {Id, string:concat("/user/", IdString)};
        {aborted, Error} ->
            log4erl:debug("Error in creating user ~s: ~p", [IdString, Error]),
            throw(Error)
    end.

%% @spec delete(int()) -> bool()
delete(Id) ->
    F = fun() ->
                [User] = mnesia:read(pfuser, Id, write),
                mnesia:write(User#pfuser{status=deleted, last_modified=util:now_int()})
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            true;
        {aborted, _} ->
            false
    end.

is_valid_id(Id) ->
    string:left(Id, 16) =:= "platformer_user_"
        andalso
        util:is_valid_uuid(string:substr(Id, 17)).