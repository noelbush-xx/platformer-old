%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Node resource.
%%
%% This module is responsible for contacting other Platformer nodes.
%%
-module(platformer.core.liaison).

-export([announce_self/0, propagate/1, propagate/2, seek_peers/0]).

-import(httpc).
-import(jsonerl).
-import(lists).
-import(log4erl).
-import(qlc).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("jsonerl.hrl").

-include_lib("platformer.hrl").

%% Announce this node to other known nodes.
announce_self() ->
    Json = list_to_binary(
             ?record_to_json(nodespec,
                             #nodespec{scheme=util:get_param(scheme),
                                       host=list_to_binary(util:get_param(ip)),
                                       port=util:get_param(port)})),
    Nodes = node:get_list(),
    log4erl:info("Announcing myself to ~B peer node(s).", [length(Nodes)]),
    announce_self(Json, Nodes).

announce_self(Json, [Node|Rest])->
    Address = node:get_address(Node),
    log4erl:info("Announcing myself to node ~p.", [Address]),
    httpc:request(post, {Address ++ "/node", [], "text/javascript", Json}, [], []),
    announce_self(Json, Rest);

announce_self(_Json, [])->
    log4erl:info("No more nodes to whom to announce myself."),
    ok.

%% Ask other nodes for their node lists.  We check with
%% the 25% least recently contacted nodes.
seek_peers() ->
    Nodes = lists:sort(fun(A, B) ->
                               A#pfnode.last_modified < B#pfnode.last_modified end,
                       node:get_list()),
    Sublist = lists:sublist(Nodes, trunc(length(Nodes) * 0.25 + 1)),
    seek_peers(Sublist).

seek_peers([Node|Rest]) ->
    Address = node:get_address(Node),
    log4erl:info("Asking node ~p for its node list.", [Address]),
    case httpc:request(Address ++ "/node/list") of
        {ok, {{_, 200, _}, _, Body}} ->
            {{<<"nodes">>, Peers}} = jsonerl:decode(Body),
            node:create_from_list(Peers);
        _ ->
            log4erl:info("Could not retrieve node list from ~p.~n", [Address])
    end,
    seek_peers(Rest);
seek_peers([]) ->
    log4erl:info("No more nodes to query for peers."),
    ok.

propagate(Json) ->
    ok.
    %%propagate(Json, util:get_param(propagation_age)).

propagate(Json, Age) ->
    propagate(node:get_random_list(Age), Json, Age).

propagate([Node|Rest], Json, Age) ->
    log4erl:info("Propagating to node ~s.", [node:get_address(Node)]),
    propagate(Rest, Json, Age);
propagate([], _, _) -> ok.
