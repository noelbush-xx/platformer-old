%% @doc Represents a node in Platformer.  (Functions here are
%% independent of any data storage mechanism.)
%%
%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

-module(platformer_node).
-behaviour(platformer_element).

% Exports required by platformer_element.
-export([get_id/1, get_path/1]).

% Other exports, specific to platformer_node.
-export([adjust_rating/2, me/0, my_id/0, my_address/0, get_address/1,
         get_other_lists/0, is_me/1, announce_self/0, seek_peers/0]).

-include_lib("jsonerl.hrl").
-include_lib("platformer.hrl").

%% @doc Constructs the id representing the given node.  A string
%% will be interpreted as an address, a record as a platformer_node record.
%%
%% @spec get_id(platformer_node() | string()) -> string()
get_id(#platformer_node{} = Record) ->
    get_id(get_address(Record));
get_id(Address) ->
    list_to_binary(string:concat("platformer_node_", platformer_util:md5(Address))).

%% @doc Returns the path that should be used (appended to hostname for URI)
%% for referring to this node.
%%
%% @spec get_path(platformer_node()) -> string()
get_path(#platformer_node{id=Id}) when Id =/= undefined ->
    "/node/" ++ binary_to_list(Id);
get_path(#platformer_node{} = Record) ->
    "/node/" ++ get_id(Record).

%% @doc Constructs the address of the given node record.
%%
%% @spec get_address(platformer_node{}) -> string()
get_address(#platformer_node{scheme=Scheme, host=Host, port=Port}) ->
    lists:concat([Scheme, "://", binary_to_list(Host), ":", Port]).

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
    platformer_node_memo:get(my_id()).

my_id() ->
    get_id(my_address()).

%% @doc Queries other known nodes for their node lists, and adds any
%% new ones found to ours.  Returns the list of newly discovered
%% nodes.
%%
%% @spec get_other_lists() -> [platformer_node()]
get_other_lists() ->
    get_other_list(platformer_node_memo:get_my_list(), []).

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
                        Records = [platformer_node_memo:from_json(S) || S <- Specs],
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
    Nodes = platformer_node_memo:get_my_list(),
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
                       platformer_node_memo:get_my_list()),
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
            platformer_node_memo:create_from_list(Peers);
        _ ->
            %% log4erl:debug("Could not retrieve node list from ~p; reducing rating.", [Address]),
            adjust_rating(Node, -1)
    end,
    seek_peers(Rest);
seek_peers([]) ->
    %% log4erl:debug("No more nodes to query for peers."),
    ok.
