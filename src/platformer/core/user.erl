%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer.core.user).

-export([create/0, delete/1, exists/1]).

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
                                      [{"X-Platformer-Node", node:my_address()}]}, [], []) of
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


%% @spec create() -> {Id, Path}
create() ->
    IdString = string:concat("platformer_user_", uuid:to_string(uuid:v4())),
    Id = list_to_binary(IdString),
    User = #pfuser{id=Id, status=active, last_modified=util:now_int()},
    case db:write(User) of
        {atomic, ok} ->
            log4erl:info("Propagating new user ~s", IdString),
            %%spawn_link(platformer.core.liaison, propagate, ?record_to_json(pfuser, User)),
            {Id, string:concat("/userid/", IdString)};
        {aborted, Error} ->
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
