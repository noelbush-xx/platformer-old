%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(pfuser).
-include_lib("stdlib/include/qlc.hrl").
-include("platformer.hrl").
-export([create/0, delete/1, exists/1]).

%% @spec exists(int(), bool()) -> {bool(), atom()}
exists({Id, CheckOtherNodes}) ->
    %% First check the local database.
    Result = platformer_db:find(qlc:q([X || X <- mnesia:table(pfuser),
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
                true -> exists(Id, pfnode:get_list());
                false -> {false, unknown}
            end;
        _ ->
            throw({codingError, "Multiple results found for id!"})
    end.
    
exists(Id, [#pfnode{} = Node|Nodes]) ->
    case pfnode:is_me(Node) of
        false ->
            case httpc:request(head, {lists:concat([pfnode:get_address(Node), "/user/", Id]),
                                      [{"X-Platformer-Node", pfnode:my_address()}]}, [], []) of
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
    Id = string:concat("platformer_user_", uuid:to_string(uuid:v4())),
    User = #pfuser{id=Id, status=active, last_modified=util:now_int()},
    case platformer_db:write(User) of
        {atomic, ok} ->
            liaison:propagate(?record_to_json(pfuser, User),
            {Id, string:concat("/userid/", Id)};
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
