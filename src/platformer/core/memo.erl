%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.
%%
%% This module contains functions used by various memo-derived
%% entities in Platformer.
-module(platformer.core.memo).

-import(httpc).
-import(lists).

-import(log4erl).

-include_lib("platformer.hrl").

-export([is_valid_token/2, is_valid_priority/2, propagate/4, propagate/5]).

is_valid_token(S, Description) ->
    case util:is_valid_uuid(S) of
        true -> {true, S};
        false -> {false, Description ++ " must be a valid v4 uuid."}
    end.

is_valid_priority(S, Description) ->
    try list_to_integer(S) of
        I ->
            case I < 0 of
                true ->
                    {false, Description ++ " must be positive."};
                false ->
                    MaxPriority = util:get_param(memo_priority_max),
                    case I > MaxPriority of
                        true ->
                            {false, Description ++ " must be no greater than " ++ integer_to_list(MaxPriority) ++ "."};
                        false ->
                            {true, I}
                    end
            end
    catch
        error:badarg -> {false, Description ++ " must be an integer."}
    end.


%% @doc Propagate an item with no body.
%%
%% @spec propagate(string(), string(), envelope()) -> ok
propagate(Action, Type, Id, #envelope{} = Envelope) when Action =:= put orelse Action =:= delete ->
    propagate(Action, Type, Id, undefined, Envelope).

%% @doc Propagate an item with a body.
%%
%% @spec propagate(string(), string(), string(), envelope()) -> ok
propagate(Action, Type, Id, Body, #envelope{priority=Priority} = Envelope) when Action =:= put orelse Action =:= delete ->
    Nodes = node:get_random_list({count, Priority}),
    log4erl:debug("Propagating priority ~B ~p of ~s ~s to ~B node(s).", [Priority, Action, Type, Id, length(Nodes)]),
    propagate(Nodes, Action, Type, Id, Body, Envelope).

propagate([Node|Rest], Action, Type, Id, Body, #envelope{token=Token, priority=Priority, source=Source} = Envelope) ->
    Address = node:get_address(Node),
    log4erl:debug("Propagating ~p of ~s to node ~s.", [Action, Type, Address]),
    Uri = lists:concat([Address, "/", Type, "/", Id]),
    Headers = [{"X-Platformer-Memo-Token", Token},
               {"X-Platformer-Memo-Priority", integer_to_list(Priority)},
               {"X-Platformer-Memo-Source", Source}],
    case Body of
        undefined ->
            httpc:request(Action, {Uri, Headers, "text/plain", ""}, [], []);
        Body ->
            httpc:request(Action, {Uri, Headers, "text/javascript", Body}, [], [])
    end,
    propagate(Rest, Action, Type, Id, Body, Envelope);
propagate([], Action, Type, Id, _, _) ->
    log4erl:debug("No more nodes to which to propagate ~p of ~s ~s.", [Action, Type, Id]),
    ok.
