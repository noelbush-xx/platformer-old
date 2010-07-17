%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc miscellaneous utilities.
-module(platformer.core.util).

-import(application).
-import(io_lib).
-import(lists).
-import(math).
-import(mochijson).
-import(random).
-import(re).
-import(string).

-import(uuid).

-export([get_param/1, get_param/2, is_valid_uuid/1, json_wrap/2, jsonify/1, md5/1, now_int/0, shuffle/1, uuid/0]).

%% From http://rosettacode.org/wiki/MD5#Erlang
md5(S) ->
    string:to_upper(
      lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])
     ).

uuid() ->
    uuid:to_string(uuid:v4()).

%% @doc Verify that the given string is a valid v4 UUID.
is_valid_uuid(String) ->
    case re:run(String, "[[:xdigit:]]{8}-[[:xdigit:]]{4}-4[[:xdigit:]]{3}-[89ab][[:xdigit:]]{3}-[[:xdigit:]]{12}", [caseless]) of
        {match, _} -> true; 
        nomatch -> false
    end.
    

%% From http://www.trapexit.org/RandomShuffle
shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    random:seed(now()),
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
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

get_param(Par) ->
    get_param(Par, undefined).
get_param(Par, Default) ->
    case application:get_env(platformer, Par) of undefined -> Default; {ok, Val} -> Val end.

%% erlang's "now()" as an integer.
now_int() ->
    {Int, _} = string:to_integer(lists:concat(tuple_to_list(now()))),
    Int.

%% Encode the given data as json and wrap it with the given prefix.
json_wrap(Prefix, Data) ->
  io_lib:format("~s(~s);", [Prefix, jsonify(Data)]).

%% json-ify the given data.
jsonify(Data) ->
  mochijson:encode({struct, Data}).
