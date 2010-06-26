%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer.core.node).

-import(application).
-import(http_uri).
-import(httpd_socket).
-import(lists).
-import(log4erl).
-import(mnesia).
-import(proplists).
-import(qlc).
-import(string).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jsonerl.hrl").
-include_lib("platformer.hrl").

-export([create/1, create_from_list/1, get/1, delete/1, load_preconfigured/0, my_address/0,
         get_address/1, get_hash/1, get_list/0, get_random_list/1, get_random_list/2, get_path/1, is_me/1]).

%% @doc Deletes a node identified by a hash.  Return value indicates
%% whether the operation succeeded.

%% @spec delete(binary()) -> bool()
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

%% @doc Creates nodes from the given list of node specifications.
%%
%% @spec create_from_list([nodespec()]) -> is_me | {ok, node()} | {error, string()}
create_from_list([Node|Rest]) ->
    create(Node),
    create_from_list(Rest);
create_from_list([]) -> ok.

%% @doc Creates a node from the given information, assuming a starting rating of 100.
%% @see create/2
%%
%% @spec create(term() | tuple() | string()) -> is_me | {ok, node()} | {error, string()}
create(NodeSpec) ->
    create(NodeSpec, 100).

%% @doc Creates a node from the given information and the specified
%%  rating.  A tuple will be parsed as a #pfnode record, first
%%  converted if necessary; a string will be parsed as an address
%%  (e.g., "http://localhost:8000").  The <code>is_me</code> return
%%  result indicates that the given specification indicates the
%%  current node and that no new node was created.
%%
%% @spec create(term() | tuple() | string(), integer()) -> is_me | {ok, node()} | {error, string()}
create(#pfnode{} = Record, Rating) ->
    case is_me(Record) of
        true ->
            log4erl:info("Will not create record for own node."),
            is_me;
        _ ->
            Address = get_address(Record),
            Hash = get_hash(Address),

            %% First check whether node record already exists in database.
            case node:get(Hash) of
                not_found ->
                    % Augment the supplied record to become a full-fledged node record.
                    Node = Record#pfnode{status=active,
                                           hash=list_to_binary(Hash),
                                           rating=Rating,
                                           last_modified=util:now_int()},
                    log4erl:info("Creating new node with address ~p.", [Address]),
                    case db:write(Node) of
                        {atomic, ok} ->
                            {ok, Node};
                        {aborted, Error} ->
                            {error, Error}
                    end;
                Node ->
                    log4erl:info("Node ~p is already known.", [Address]),
                    {already_exists, Node}
            end
    end;
create(Tuple, Rating) when is_tuple(Tuple) ->
    Proplist = [{binary_to_atom(X, latin1), Y} || {X, Y} <- tuple_to_list(Tuple)],
    RawRecord = list_to_tuple([pfnode|[proplists:get_value(X, Proplist) || X <- record_info(fields, pfnode)]]),
    Record = RawRecord#pfnode{scheme=binary_to_atom(RawRecord#pfnode.scheme, latin1)},
    create(Record, Rating);
create(Address, Rating) when is_list(Address) ->
    {Scheme, _UserInfo, Host, Port, _Path, _Query} = http_uri:parse(Address),
    create(#pfnode{scheme=Scheme, host=list_to_binary(Host), port=Port}, Rating).

%% @doc Constructs the address of the given node record.
%%
%% @spec get_address(#pfnode{}) -> string()
get_address(#pfnode{scheme=Scheme, host=Host, port=Port}) ->
    lists:concat([Scheme, "://", binary_to_list(Host), ":", Port]).

%% @doc Constructs the hash code representing the given node.  A string
%% will be interpreted as an address, a record as a pfnode record.
%%
%% @spec get_hash(NodeRecord | string()) -> string()
get_hash(#pfnode{} = Record) ->
    get_hash(get_address(Record));
get_hash(Address) ->
    util:md5(Address).

%% @doc Returns the path that should be used (appended to hostname for URI)
%% for referring to this node.
%%
%% @spec get_path(NodeRecord) -> string()
get_path(#pfnode{hash=Hash}) when Hash =/= undefined ->
    "/node/" ++ binary_to_list(Hash);
get_path(#pfnode{} = Record) ->
    "/node/" ++ get_hash(Record).

%% @doc Indicates whether the given node record refers to the current node.
%%
%% @spec is_me(pfnode()) -> bool()
is_me(#pfnode{host=Host, port=Port}) ->
    case binary_to_list(Host) =:= util:get_param(ip) of
        true -> Port =:= util:get_param(port);
        false -> false
    end.

%% @doc Returns a list of all nodes known to this one.
%%
%% @spec get_list() -> [NodeRecord]
get_list() ->
    db:read_all(pfnode).

%% @doc Retrieves a node from its hash code.
%%
%% @spec get(string()) -> NodeRecord
get(Hash) ->
    Result = db:find(qlc:q([X || X <- mnesia:table(pfnode), X#pfnode.hash == list_to_binary(Hash)])),
    case length(Result) of
        1 ->
            hd(Result);
        0 ->
            not_found
    end.

%% @doc Gets a random list of nodes known to this node.  The
%% <code>SampleSize</code> may be specified as <code>{percentage,
%% Value}</code> (0-100) or <code>{count, Count}</code> (> 0).
%%
%% @spec get_random_list(tuple()) -> [NodeRecord]
get_random_list(SampleSize) -> get_random_list(SampleSize, []).


%% @doc Same as {@link get_random_list/1}, with an additional
%% parameter <code>Criteria</code>, a list that may contain one, none,
%% or both of <code>{min_rating, Rating}</code> or <code>{max_rating,
%% Rating}</code> where <code>Rating</code> is between 0 and 100,
%% inclusive.
%%
%% @spec get_random_list(tuple(), [tuple()]) -> [NodeRecord]
get_random_list({percentage, Percentage}, _Criteria) when Percentage < 0; Percentage > 100 ->
    throw({error, "Sample size percentage must be between 0 and 100."});

get_random_list({count, Count}, _Criteria) when Count < 0 ->
    throw({error, "Sample size count must be greater than 0."});

get_random_list(SampleSize, Criteria) ->
    MinRating = proplists:get_value(min_rating, Criteria, 0),
    MaxRating = proplists:get_value(max_rating, Criteria, 100),
    case db:find(qlc:q([X || X <- mnesia:table(pfnode), X#pfnode.rating >= MinRating, X#pfnode.rating =< MaxRating])) of
        undefined -> [];
        [] -> [];
        Nodes ->
            SublistSize = 
                case SampleSize of
                    {percentage, Percentage} -> trunc(length(Nodes) * Percentage / 100 + 1);
                    {count, Count} -> lists:min([length(Nodes), Count])
                end,
            Choices = lists:sublist(util:shuffle(Nodes), SublistSize),
            [?record_to_struct(pfnode, Node) || Node <- Choices]
    end.

    
%% @doc Ensures any preconfigured nodes are in the database.
%%
%% @spec load_preconfigured() -> ok
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

%% @doc Returns the address of this node -- i.e., a string consisting
%% of "http" or "https" followed by "://", then the hostname, then ":"
%% and the port number.
%%
%% @spec my_address() -> string()
my_address() ->
    lists:concat(["http://", util:get_param(ip, httpd_socket:resolve()), ":", util:get_param(port, 2010)]).
