%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Node resource.
%%
%% This module is responsible for contacting other Platformer nodes.
%%
-module(liaison).

-export([announce_self/0, seek_peers/0]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("external/jsonerl/jsonerl.hrl").

-include("platformer.hrl").

%% Announce this node to other known nodes.
announce_self() ->
    Json = list_to_binary(?record_to_json(pfnode, #pfnode{scheme=util:get_param(scheme),
                                           host=list_to_binary(util:get_param(ip)), port=util:get_param(port)})),
    Nodes = pfnode:get_list(),
    log4erl:info("Announcing myself to ~B peer node(s).", [length(Nodes)]),
    io:format("Nodes: ~p~n", [Nodes]),
    announce_self(Json, Nodes).

announce_self(Json, [Node|Rest])->
    Address = pfnode:get_address(Node),
    log4erl:info("Announcing myself to node ~p.", [Address]),
    httpc:request(post, {Address ++ "/node", [], "text/javascript", Json}, [], []),
    announce_self(Json, Rest);

announce_self(_Json, [])->
    log4erl:info("No more nodes to whom to announce myself."),
    ok.

%% Ask other nodes for their node lists.  We check with
%% the 25% least recently contacted nodes.
seek_peers() ->
    Nodes = lists:sort(fun(A, B) -> A#pfnode.last_modified < B#pfnode.last_modified end, pfnode:get_list()),
    Sublist = lists:sublist(Nodes, trunc(length(Nodes) * 0.25 + 1)),
    seek_peers(Sublist).

seek_peers([Node|Rest]) ->
    Address = pfnode:get_address(Node),
    log4erl:info("Asking node ~p for its node list.", [Address]),
    case httpc:request(Address ++ "/node/list") of
        {ok, {{_, 200, _}, _, Body}} ->
            {{<<"nodes">>, Peers}} = jsonerl:decode(Body),
            pfnode:create_from_list(Peers);
        _ ->
            log4erl:info("Could not retrieve node list from ~p.~n", [Address])
    end,
    seek_peers(Rest);
seek_peers([]) ->
    log4erl:info("No more nodes to query for peers."),
    ok.
