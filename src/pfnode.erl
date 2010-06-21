%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(pfnode).
-include_lib("stdlib/include/qlc.hrl").
-include("platformer.hrl").
-export([create/1, create_from_list/1, get/1, delete/1, load_preconfigured/0, my_address/0,
         get_address/1, get_hash/1, get_list/0, get_path/1, is_me/1]).

%% @spec delete_userid(int()) -> bool()
delete(Hash) ->
    F = fun() ->
                [Node] = mnesia:read(pfuser, Hash, write),
                mnesia:write(Node#pfnode{status=deleted, last_modified=util:now_int()})
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            true;
        {aborted, _} ->
            false
    end.

create_from_list([Node|Rest]) ->
    create(Node),
    create_from_list(Rest);
create_from_list([]) -> ok.

create(#pfnode{} = Record) ->
    create(Record, 100);
create(Tuple) when is_tuple(Tuple) ->
    Proplist = [{binary_to_atom(X, latin1), Y} || {X, Y} <- tuple_to_list(Tuple)],
    RawRecord = list_to_tuple([pfnode|[proplists:get_value(X, Proplist) || X <- record_info(fields, pfnode)]]),
    Record = RawRecord#pfnode{scheme=binary_to_atom(RawRecord#pfnode.scheme, latin1)},
    create(Record);
create(Address) when is_list(Address) ->
    {Scheme, _UserInfo, Host, Port, _Path, _Query} = http_uri:parse(Address),
    create(#pfnode{scheme=Scheme, host=list_to_binary(Host), port=Port}).

create(#pfnode{} = Record, Rating) ->
    case is_me(Record) of
        true ->
            log4erl:info("Will not create record for own node."),
            is_me;
        _ ->
            Address = get_address(Record),
            Hash = get_hash(Address),

            %% First check whether node record already exists in database.
            case pfnode:get(Hash) of
                not_found ->
                    % Augment the supplied record to become a full-fledged node record.
                    Node = Record#pfnode{status=active,
                                           hash=list_to_binary(Hash),
                                           rating=Rating,
                                           last_modified=util:now_int()},
                    log4erl:info("Creating new node with address ~p.", [Address]),
                    case platformer_db:write(Node) of
                        {atomic, ok} ->
                            {ok, Node};
                        {aborted, Error} ->
                            {error, Error}
                    end;
                Node ->
                    log4erl:info("Node ~p is already known.", [Address]),
                    {already_exists, Node}
            end
    end.

get_address(#pfnode{scheme=Scheme, host=Host, port=Port}) ->
    lists:concat([Scheme, "://", binary_to_list(Host), ":", Port]).

get_hash(#pfnode{} = Record) ->
    get_hash(get_address(Record));
get_hash(Address) ->
    util:md5(Address).

get_path(#pfnode{hash=Hash}) when Hash =/= null ->
    "/node/" ++ binary_to_list(Hash);
get_path(#pfnode{} = Record) ->
    "/node/" ++ get_hash(Record).

%% Is the given node "me" (the current node)?
is_me(#pfnode{host=Host, port=Port}) ->
    case binary_to_list(Host) =:= util:get_param(ip) of
        true -> Port =:= util:get_param(port);
        false -> false
    end.
%% Return all nodes.
get_list() ->
    platformer_db:read_all(pfnode).

get(Hash) ->
    Result = platformer_db:find(qlc:q([X || X <- mnesia:table(pfnode), X#pfnode.hash == list_to_binary(Hash)])),
    case length(Result) of
        1 ->
            hd(Result);
        0 ->
            not_found
    end.
    
%% Ensure any preconfigured nodes are in the database.
load_preconfigured() ->
    case application:get_env(platformer, nodes) of
        {ok, Nodes} ->
            log4erl:info("Loading ~B preconfigured node(s) from app config.", [length(Nodes)]),
            load_preconfigured(Nodes);
        _ ->
            ok
    end.

load_preconfigured([Address|Rest]) ->
    log4erl:debug("Preconfigured node: ~p", [Address]),
    create(Address),
    load_preconfigured(Rest);
load_preconfigured([]) -> ok.

my_address() ->
    lists:concat(["http://", util:get_param(ip, httpd_socket:resolve()), ":", util:get_param(port, 2010)]).
