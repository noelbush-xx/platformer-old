%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Server resource.
%%
%% This module is responsible for contacting other Platformer nodes.
%%
-module(liaison).

-export([announce_self/0, seek_peers/0]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("external/jsonerl/jsonerl.hrl").

-include("platformer.hrl").

-import(server_resource, [get_address/1, new_servers/1]).

%% Announce this node to other known nodes.
announce_self() ->
    Json = list_to_binary(?record_to_json(server, #server{scheme=util:get_param(scheme),
                                           host=list_to_binary(util:get_param(ip)), port=util:get_param(port)})),
    announce_self(Json, server_resource:get_list()).

announce_self(Json, [Server|Rest])->
    Address = get_address(Server),
    log4erl:info("Announcing myself to server ~p.", [Address]),
    httpc:request(post, {Address ++ "/server", [], "text/javascript", Json}, [], []),
    announce_self(Json, Rest);

announce_self(_Json, [])->
    log4erl:info("No more servers to whom to announce myself."),
    ok.

%% Ask other nodes for their server lists.  We check with
%% the 25% least recently contacted servers.
seek_peers() ->
    Servers = lists:sort(fun(A, B) -> A#server.last_modified < B#server.last_modified end, server_resource:get_list()),
    Sublist = lists:sublist(Servers, trunc(length(Servers) * 0.25 + 1)),
    seek_peers(Sublist).

seek_peers([Server|Rest]) ->
    Address = get_address(Server),
    log4erl:info("Asking server ~p for its server list.", [Address]),
    case httpc:request(Address ++ "/server/list") of
        {ok, {{_, 200, _}, _, Body}} ->
            {{<<"servers">>, Peers}} = jsonerl:decode(Body),
            new_servers(Peers);
        _ ->
            log4erl:info("Could not retrieve server list from ~p.~n", [Address])
    end,
    seek_peers(Rest);
seek_peers([]) ->
    log4erl:info("No more servers to query for peers."),
    ok.
