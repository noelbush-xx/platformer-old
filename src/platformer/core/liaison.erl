%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Node resource.
%%
%% This module is responsible for contacting other Platformer nodes.
%%
-module(platformer.core.liaison).

-export([announce_self/0, propagate/3, propagate/4, seek_peers/0]).

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
    %% log4erl:debug("Announcing myself to ~B peer node(s).", [length(Nodes)]),
    announce_self(Json, Nodes).

announce_self(Json, [Node|Rest])->
    Address = node:get_address(Node),
    %% log4erl:debug("Announcing myself to node ~p.", [Address]),
    httpc:request(post, {Address ++ "/node", [], "text/javascript", Json}, [], []),
    announce_self(Json, Rest);

announce_self(_Json, [])->
    %% log4erl:debug("No more nodes to whom to announce myself."),
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
    %% log4erl:debug("Asking node ~p for its node list.", [Address]),
    case httpc:request(Address ++ "/node/list") of
        {ok, {{_, 200, _}, _, Body}} ->
            %% log4erl:debug("Retrieved node list from ~p; increasing rating.", [Address]),
            node:adjust_rating(Node, 1),
            {{<<"nodes">>, Peers}} = jsonerl:decode(Body),
            node:create_from_list(Peers);
        _ ->
            %% log4erl:debug("Could not retrieve node list from ~p; reducing rating.", [Address]),
            node:adjust_rating(Node, -1)
    end,
    seek_peers(Rest);
seek_peers([]) ->
    %% log4erl:debug("No more nodes to query for peers."),
    ok.

%% @doc Propagate an item with no body.
%%
%% @spec propagate(string(), string(), {string(), string(), string()}) -> ok
propagate(Type, Id, {_Token, _Priority, _Source} = Envelope) ->
    propagate(Type, Id, undefined, Envelope).

%% @doc Propagate an item with a body.
%%
%% @spec propagate(string(), string(), string(), {string(), string(), string()}) -> ok
propagate(Type, Id, Body, {_Token, Priority, _Source} = Envelope) ->
    Nodes = node:get_random_list({count, Priority}),
    log4erl:debug("Propagating priority ~B ~s ~s to ~B node(s).", [Priority, Type, Id, length(Nodes)]),
    propagate(Nodes, Type, Id, Body, Envelope).

propagate([Node|Rest], Type, Id, Body, {Token, Priority, Source} = Envelope) ->
    Address = node:get_address(Node),
    log4erl:debug("Propagating ~s to node ~s.", [Type, Address]),
    Uri = lists:concat([Address, "/", Type, "/", Id]),
    Headers = [{"X-Platformer-Message-Token", Token},
               {"X-Platformer-Message-Priority", Priority},
               {"X-Platformer-Message-Source", Source}],
    case Body of
        undefined ->
            httpc:request(put, {Uri, Headers, "text/plain", ""}, [], []);
        Body ->
            httpc:request(put, {Uri, Headers, "text/javascript", Body}, [], [])
    end,
    propagate(Rest, Type, Id, Body, Envelope);
propagate([], Type, Id, _, _) ->
    log4erl:debug("No more nodes to which to propagate ~s ~s.", [Type, Id]),
    ok.
