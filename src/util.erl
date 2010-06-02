%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc miscellaneous utilities.
-module(util).

-export([get_param/2, md5/1, shuffle/1]).

%% From http://rosettacode.org/wiki/MD5#Erlang
md5(S) ->
    string:to_upper(
      lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])
     ).

%% From http://www.trapexit.org/RandomShuffle
shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    random:seed(now()),
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                        randomize(Acc)
                end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) ->
                          {random:uniform(), A}
                  end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)), 
    D1.

get_param(Par, Default) ->
    case application:get_env(Par) of undefined -> Default; {ok, Val} -> Val end.
            
