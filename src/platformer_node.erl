%% @doc Represents a node in Platformer.  Implements the memo behavior
%% so information about nodes can be passed around like other
%% information in Platformer.
%%
%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer_node).
-behaviour(platformer_memo).

%% Exports required by platformer_memo.
-export([create/2, delete/2, exists/1, exists/2, get/1, is_valid_id/1, to_json/1]).

%% Other exports specific to nodes.
-export([adjust_rating/2, create/1, create_from_list/1, from_json/1, me/0, my_id/0,
         load_preconfigured/0, my_address/0, get_address/1,
         get_id/1, get_list/0, get_list/1, get_random_list/1, get_random_list/2,
         get_random_list/3, get_other_lists/0, get_path/1, is_me/1, announce_self/0,
         seek_peers/0]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jsonerl.hrl").
-include_lib("platformer.hrl").


%% @spec create(string()) -> {Id::string(), Path::string()}
create(Address) when is_list(Address) ->
    case http_uri:parse(Address) of
        {Scheme, _UserInfo, Host, Port, _Path, _Query} ->
            create(#platformer_node{scheme=Scheme, host=list_to_binary(Host), port=Port});
        {error, Error} ->
            log4erl:error("Could not parse address ~s.  Error: ~p", [Address, Error])
    end;

%% @spec create(platformer_node()) -> {Id::string(), Path::string()}
create(#platformer_node{} = Record) ->
    log4erl:debug("Creating new node from record: ~p", [Record]),
    create(Record, #envelope{priority=0});
create(RecordData) when is_tuple(RecordData) ->
    RawRecord = platformer_util:record_from_proptuple(platformer_node, RecordData),
    create(RawRecord#platformer_node{scheme=binary_to_atom(RawRecord#platformer_node.scheme, latin1)}).

%% @spec create(platformer_node(), envelope()) -> {Id::string(), Path::string()}
create(#platformer_node{} = Record, #envelope{} = Envelope) ->
    Id = get_id(Record),
    Node = Record#platformer_node{status=active,
                                  id=Id,
                                  rating=case Record#platformer_node.rating of undefined -> 100; Rating -> Rating end,
                                  last_modified=platformer_util:now_int()},
    %% The envelope is basically meaningless; be sure priority is 0 so we don't try to propagate.
    log4erl:debug("Created new node record: ~p", [Node]),
    platformer_memo:create("node", Id, Node, Envelope#envelope{priority=0}).

delete(Id, #envelope{} = Envelope) ->
    platformer_memo:delete("node", Id, Envelope#envelope{priority=0}).

%% @doc Check whether there is a local record for a node with the given id.
%%
%% @spec exists(binary()) -> {bool(), active | deleted}
exists(Id) when is_binary(Id) ->
    platformer_memo:exists("node", Id).

%% @doc Since node records only matter locally, returns the same as {@link exists/1}.
%%    
%% @spec exists(binary(), envelope()) -> {bool(), active | deleted}
exists(Id, #envelope{} = _Envelope) when is_binary(Id) ->
    exists(Id).

%% @doc Get a node record by id.
%%
%% @spec get(binary()) -> platformer_node()
get(Id) ->
    platformer_memo:get("node", Id).


%% @doc Produce a json representation of the node with the given id.
%%
%% @spec to_json(binary()) -> string()
to_json(Id) when is_binary(Id) ->
    platformer_memo:to_json(node, Id).

%% @doc Is the given id valid for a node record?
%%
%% @spec is_valid_id(binary()) -> bool()
is_valid_id(Id) when is_binary(Id) ->
    platformer_memo:is_valid_id("node", Id).

from_json(Json) ->
    %% log4erl:debug("Convert from json: ~s", [Json]),
    try ?json_to_record(platformer_node, Json) of
        #platformer_node{scheme=Scheme, status=Status} = Record ->
            Record#platformer_node{scheme = binary_to_atom(Scheme, latin1), status = binary_to_atom(Status, latin1)}
    catch _:Error ->
            log4erl:debug("Error converting record from json: ~p", [Error]),
            error
    end.

%% @doc Constructs the address of the given node record.
%%
%% @spec get_address(#platformer_node{}) -> string()
get_address(#platformer_node{scheme=Scheme, host=Host, port=Port}) ->
    lists:concat([Scheme, "://", binary_to_list(Host), ":", Port]).

%% @doc Constructs the id representing the given node.  A string
%% will be interpreted as an address, a record as a platformer_node record.
%%
%% @spec get_id(NodeRecord | string()) -> string()
get_id(#platformer_node{} = Record) ->
    get_id(get_address(Record));
get_id(Address) ->
    list_to_binary(string:concat("platformer_node_", platformer_util:md5(Address))).

%% @doc Returns the path that should be used (appended to hostname for URI)
%% for referring to this node.
%%
%% @spec get_path(NodeRecord) -> string()
get_path(#platformer_node{id=Id}) when Id =/= undefined ->
    "/node/" ++ binary_to_list(Id);
get_path(#platformer_node{} = Record) ->
    "/node/" ++ get_id(Record).

%% @doc Indicates whether the given node record refers to the current node.
%%
%% @spec is_me(platformer_node()) -> bool()
is_me(#platformer_node{host=Host, port=Port}) ->
    is_me(binary_to_list(Host), Port);
is_me(Address) ->
    case http_uri:parse(Address) of
        {_Scheme, _UserInfo, Host, Port, _Path, _Query} ->
            is_me(Host, Port);
        {error, {malformed_url, AbsURI}} -> {error, {malformed_url, AbsURI}}
    end.
is_me(Host, Port) ->
    case Host =:= platformer_util:get_param(ip) of
        true -> Port =:= platformer_util:get_param(port);
        false -> false
    end.    

me() ->
    platformer_node:get(my_id()).

my_id() ->
    get_id(my_address()).

%% @doc Creates nodes from the given list of node specifications.
%%
%% @spec create_from_list([nodespec()]) -> is_me | {ok, node()} | {error, string()}
create_from_list([Node|Rest]) ->
    create(Node),
    create_from_list(Rest);
create_from_list([]) -> ok.

%% @doc Returns a list of all nodes known to this one,
%% <em>including</em> the present node.
%%
%% @spec get_list(bool()) -> [NodeRecord]
get_list(include_self) ->
    platformer_db:read_all(platformer_node).

%% @doc Returns a list of all nodes known to this one, <em>except</em>
%% the present node itself.
get_list() ->
    MyId = my_id(),
    platformer_db:find(qlc:q([X || X <- mnesia:table(platformer_node),
                                   X#platformer_node.id =/= MyId])).

%% @doc Gets a random list of nodes known to this node.  The
%% <code>SampleSize</code> may be specified as <code>{percentage,
%% Value}</code> (0-100) or <code>{count, Count}</code> (> 0).
%%
%% @spec get_random_list(tuple()) -> [NodeRecord]
get_random_list(SampleSize) -> get_random_list(SampleSize, [], []).

get_random_list(SampleSize, Criteria) -> get_random_list(SampleSize, Criteria, []).

%% @doc Same as {@link get_random_list/1}, with an additional
%% parameter <code>Criteria</code>, a list that may contain one, none,
%% or both of <code>{min_rating, Rating}</code> or <code>{max_rating,
%% Rating}</code> where <code>Rating</code> is between 0 and 100,
%% inclusive.
%%
%% @spec get_random_list(tuple(), [tuple()], [tuple()]) -> [NodeRecord]
get_random_list({percentage, Percentage}, _, _) when Percentage < 0; Percentage > 100 ->
    throw({error, "Sample size percentage must be between 0 and 100."});

get_random_list({count, Count}, _, _) when Count < 0 ->
    throw({error, "Sample size count must be greater than 0."});

get_random_list(SampleSize, Criteria, Omit) ->
    MinRating = proplists:get_value(min_rating, Criteria, 0),
    MaxRating = proplists:get_value(max_rating, Criteria, 100),
    case platformer_db:find(qlc:q([X || X <- mnesia:table(platformer_node),
                             X#platformer_node.rating >= MinRating,
                             X#platformer_node.rating =< MaxRating,
                             not lists:member(X#platformer_node.id, Omit)])) of
        undefined -> [];
        [] -> [];
        Nodes ->
            SublistSize = 
                case SampleSize of
                    {percentage, Percentage} -> trunc(length(Nodes) * Percentage / 100 + 1);
                    {count, Count} -> lists:min([length(Nodes), Count]);
                    _ -> throw({error, "Invalid value for SampleSize"})
                end,
            lists:sublist(platformer_util:shuffle(Nodes), SublistSize)
    end.

%% @doc Queries other known nodes for their node lists, and adds any
%% new ones found to ours.  Returns the list of newly discovered
%% nodes.
%%
%% @spec get_other_lists() -> [platformer_node()]
get_other_lists() ->
    get_other_list(get_list(), []).

get_other_list([Node|Rest], Acc) ->
    Address = get_address(Node),
    Others =
        case httpc:request(Address ++ "/node/list") of
            {ok, {{_, 200, _}, _Headers, Body}} ->
                log4erl:debug("Body: ~s", [Body]),
                case re:run(Body, "^\\{\"nodes\":\\[(.+)\\]\\}\$", [{capture, all_but_first, list}]) of
                    nomatch ->
                        log4erl:debug("Could not parse node specs from body."),
                        [];
                    {match, [Captured]} ->
                        Specs = re:split(Captured, "(\\}),", [{return, list}]),
                        log4erl:debug("Specs: ~s", [Specs]),
                        Records = [from_json(S) || S <- Specs],
                        log4erl:debug("Records: ~p", [Records]),
                        lists:filter(fun(R) -> R =/= error end, Records)
                end;
            Result ->
                log4erl:error("Could not get node list from ~s. Result:~n~p", [Address, Result]),
                []
        end,
    log4erl:debug("Got ~B other nodes from node ~s:~n~p", [length(Others), Others]),
    get_other_list(Rest, [Others|Acc]);
get_other_list([], Acc) -> lists:flatten(Acc).

    
%% @doc Ensures any preconfigured nodes are in the database.
%%
%% @spec load_preconfigured() -> ok
load_preconfigured() ->
    case application:get_env(platformer, seeds) of
        {ok, Nodes} ->
            log4erl:info("Loading ~B preconfigured node(s) from app config.", [length(Nodes)]),
            load_preconfigured(Nodes);
        _ ->
            ok
    end.

load_preconfigured([Address|Rest]) ->
    %% log4erl:debug("Preconfigured platformer_node: ~p", [Address]),
    create(Address),
    load_preconfigured(Rest);
load_preconfigured([]) -> ok.

%% @doc Returns the address of this node -- i.e., a string consisting
%% of "http" or "https" followed by "://", then the hostname, then ":"
%% and the port number.
%%
%% @spec my_address() -> string()
my_address() ->
    lists:concat(["http://", platformer_util:get_param(ip, httpd_socket:resolve()), ":", platformer_util:get_param(port, 2010)]).

adjust_rating(Node, Adjustment) ->
    CurrentRating = Node#platformer_node.rating,
    NewRating = lists:min([100, lists:max([0, CurrentRating + Adjustment])]),
    if
        NewRating =/= CurrentRating ->
            %% log4erl:debug("Adjusting rating for node from ~B to ~B.", [CurrentRating, NewRating]),
            platformer_db:write(Node#platformer_node{rating=NewRating});
        NewRating =:= CurrentRating ->
            ok
    end.

%% @doc Announce this node to other known nodes.
announce_self() ->
    Json = list_to_binary(?record_to_json(platformer_node, me())),
    Nodes = get_list(),
    log4erl:debug("Announcing myself to ~B peer node(s) with this json:~n~s", [length(Nodes), Json]),
    announce_self(Json, Nodes).

announce_self(Json, [Node|Rest])->
    Address = get_address(Node),
    log4erl:debug("Announcing myself to node ~p.", [Address]),
    httpc:request(post, {Address ++ "/node", [], "text/javascript", Json}, [], []),
    announce_self(Json, Rest);

announce_self(_Json, [])->
    log4erl:debug("No more nodes to whom to announce myself."),
    ok.

%% @doc Ask other nodes for their node lists.  We check with
%% the 25% least recently contacted nodes.
seek_peers() ->
    Nodes = lists:sort(fun(A, B) ->
                               A#platformer_node.last_modified < B#platformer_node.last_modified end,
                       get_list()),
    Sublist = lists:sublist(Nodes, trunc(length(Nodes) * 0.25 + 1)),
    seek_peers(Sublist).

seek_peers([Node|Rest]) ->
    Address = get_address(Node),
    log4erl:debug("Asking node ~p for its node list.", [Address]),
    case httpc:request(Address ++ "/node/list") of
        {ok, {{_, 200, _}, _, Body}} ->
            %% log4erl:debug("Retrieved node list from ~p; increasing rating.", [Address]),
            adjust_rating(Node, 1),
            {{<<"nodes">>, Peers}} = jsonerl:decode(Body),
            create_from_list(Peers);
        _ ->
            %% log4erl:debug("Could not retrieve node list from ~p; reducing rating.", [Address]),
            adjust_rating(Node, -1)
    end,
    seek_peers(Rest);
seek_peers([]) ->
    %% log4erl:debug("No more nodes to query for peers."),
    ok.
