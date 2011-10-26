%% @doc Implements the memo behavior for Platformer nodes, as well as
%% some additional CRUD-related tasks specific to node memos.
%%
%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer_node_memo).
-behaviour(platformer_memo).

%% Exports required by platformer_memo.
-export([create/2, delete/2, exists/1, exists/2, get/1, is_valid_id/1, to_json/1]).

%% Other exports specific to node memos.
-export([adjust_rating/2, create/1, create_from_list/1, from_json/1,
         load_preconfigured/0, get_my_list/0, get_my_list/1, get_random_list/1,
         get_random_list/2, get_random_list/3]).

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
%% @see platformer_memo:create/2.
create(#platformer_node{} = Record, #envelope{} = Envelope) ->
    Id = platformer_node:get_id(Record),
    Node = Record#platformer_node{status=active,
                                  id=Id,
                                  rating=case Record#platformer_node.rating of undefined -> 100; Rating -> Rating end,
                                  last_modified=platformer_util:now_int()},
    %% The envelope is basically meaningless; be sure priority is 0 so we don't try to propagate.
    log4erl:debug("Created new node record: ~p", [Node]),
    platformer_memo:create("node", Id, Node, Envelope#envelope{priority=0}).

%% @see platformer_memo:delete/2.
delete(Id, #envelope{} = Envelope) ->
    platformer_memo:delete("node", Id, Envelope#envelope{priority=0}).

%% @spec exists(binary()) -> {bool(), active | deleted}
%% @see platformer_memo:exists/1.
exists(Id) when is_binary(Id) ->
    platformer_memo:exists("node", Id).

%% @doc Since node records only matter locally, returns the same as {@link exists/1}.
%%    
%% @spec exists(binary(), envelope()) -> {bool(), active | deleted}
%% @see platformer_memo:exists/2.
exists(Id, #envelope{} = _Envelope) when is_binary(Id) ->
    exists(Id).

%% @spec get(binary()) -> platformer_node()
%% @see platformer_memo:get/1.
get(Id) ->
    platformer_memo:get("node", Id).

%% @spec is_valid_id(binary()) -> bool()
%% @see platformer_memo:is_valid_id/1.
is_valid_id(Id) when is_binary(Id) ->
    platformer_memo:is_valid_id("node", Id).

%% @spec to_json(binary()) -> string()
%% @see platformer_memo:to_json/1.
to_json(Id) when is_binary(Id) ->
    platformer_memo:to_json(node, Id).

%% @spec from_json(string()) -> platformer_node()
%% @see platformer_memo:from_json/1.
from_json(Json) ->
    %% log4erl:debug("Convert from json: ~s", [Json]),
    try ?json_to_record(platformer_node, Json) of
        #platformer_node{scheme=Scheme, status=Status} = Record ->
            Record#platformer_node{scheme = binary_to_atom(Scheme, latin1), status = binary_to_atom(Status, latin1)}
    catch _:Error ->
            log4erl:debug("Error converting record from json: ~p", [Error]),
            error
    end.

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
%% @spec get_my_list(include_self) -> [platformer_node()]
get_my_list(include_self) ->
    platformer_db:read_all(platformer_node).

%% @doc Returns a list of all nodes known to this one, <em>except</em>
%% the present node itself.
%%
%% @spec get_my_list() -> [platformer_node()]
get_my_list() ->
    MyId = platformer_node:my_id(),
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
    %% log4erl:debug("Preconfigured platformer_node_memo: ~p", [Address]),
    create(Address),
    load_preconfigured(Rest);
load_preconfigured([]) -> ok.

adjust_rating(Node, Adjustment) ->
    CurrentRating = Node#platformer_node.rating,
    NewRating = lists:min([100, lists:max([0, CurrentRating + Adjustment])]),
    if
        NewRating =/= CurrentRating ->
            %% log4erl:debug("Adjusting rating for node from ~B to ~B.", [CurrentRating, NewRating]),
            platformer_memo:update(Node#platformer_node{rating=NewRating});
        NewRating =:= CurrentRating ->
            ok
    end.
