%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc miscellaneous utilities.
-module(platformer_util).

-export([get_param/1, get_param/2, is_valid_uuid/1, json_wrap/2, jsonify/1, md5/1, now_int/0, shuffle/1, uuid/0,
        httpc_standard_http_options/0, httpc_standard_options/0, record_from_proptuple/2]).

-include_lib("platformer.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, exprecs}).

%% Be sure to export all record types here for which record_from_proptuple should work.
-export_records([platformer_user, platformer_node]).

%% From http://rosettacode.org/wiki/MD5#Erlang
md5(S) ->
    string:to_upper(
      lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])
     ).

uuid() ->
    random:seed(now()),
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

httpc_standard_http_options() -> [{timeout, platformer_util:get_param(httpc_timeout)}].

httpc_standard_options() -> [].
    
record_from_proptuple(Type, Data) when is_atom(Type), is_tuple(Data) ->
    list_to_tuple(
      [Type |
       lists:map(
         fun(A) -> proplists:get_value(A,
                                       lists:map(
                                         fun({K, V}) ->
                                                 {if is_binary(K) -> binary_to_atom(K, latin1);
                                                     is_list(K) -> list_to_atom(K);
                                                     true -> K
                                                  end, V}
                                         end,
                                         tuple_to_list(Data)))
         end,
         % can't use record_info at runtime -- desired record types must be exported for parse transform!
         platformer_util:'#info-'(Type))
      ]).
    

%%
%% TESTS
%%

%% Note that the tests of record_from_proptuple use a slightly
%% malformed platformer_node, in that the scheme member is not
%% converted to an atom.  We have no generic way of testing type
%% correctness for record members, so users of this function will need
%% to know to check/convert.

%% test successful conversion
record_from_proptuple_succeed_00_test() ->
    ?assertEqual(#platformer_node{id = <<"platformer_node_5931B0A7C97D3F21A8AA6EAB5D2601A1">>,
                                  scheme = <<"http">>,host = <<"0.0.0.0">>,port = 8000,
                                  status = <<"active">>,rating = 100,
                                  last_modified = 1287423906298149.0},
                 record_from_proptuple(platformer_node,
                                       {{<<"id">>,
                                         <<"platformer_node_5931B0A7C97D3F21A8AA6EAB5D2601A1">>},
                                        {<<"scheme">>,<<"http">>},
                                        {<<"host">>,<<"0.0.0.0">>},
                                        {<<"port">>,8000},
                                        {<<"status">>,<<"active">>},
                                        {<<"rating">>,100},
                                        {<<"last_modified">>,1287423906298149.0}})).

%% test as above but with list-strings instead of binary-strings (just in case)
record_from_proptuple_succeed_01_test() ->
    ?assertEqual(#platformer_node{id = "platformer_node_5931B0A7C97D3F21A8AA6EAB5D2601A1",
                                  scheme = "http",host = "0.0.0.0",port = 8000,
                                  status = "active",rating = 100,
                                  last_modified = 1287423906298149.0},
                 record_from_proptuple(platformer_node,
                                       {{"id",
                                         "platformer_node_5931B0A7C97D3F21A8AA6EAB5D2601A1"},
                                        {"scheme","http"},
                                        {"host","0.0.0.0"},
                                        {"port",8000},
                                        {"status","active"},
                                        {"rating",100},
                                        {"last_modified",1287423906298149.0}})).

%% test with misordered input data
record_from_proptuple_succeed_02_test() ->
    ?assertEqual(#platformer_node{id = <<"platformer_node_5931B0A7C97D3F21A8AA6EAB5D2601A1">>,
                                  scheme = <<"http">>,host = <<"0.0.0.0">>,port = 8000,
                                  status = <<"active">>,rating = 100,
                                  last_modified = 1287423906298149.0},
                 record_from_proptuple(platformer_node,
                                       {{<<"id">>,
                                         <<"platformer_node_5931B0A7C97D3F21A8AA6EAB5D2601A1">>},
                                        {<<"host">>,<<"0.0.0.0">>},
                                        {<<"port">>,8000},
                                        {<<"scheme">>,<<"http">>},
                                        {<<"rating">>,100},
                                        {<<"status">>,<<"active">>},
                                        {<<"last_modified">>,1287423906298149.0}})).

%% test with deliberate misspelling in a key name
record_from_proptuple_fail_00_test() ->
    ?assert(#platformer_node{id = <<"platformer_node_5931B0A7C97D3F21A8AA6EAB5D2601A1">>,
                             scheme = <<"http">>,host = <<"0.0.0.0">>,port = 8000,
                             status = <<"active">>,rating = 100,
                             last_modified = 1287423906298149.0} =/=
                record_from_proptuple(platformer_node,
                                      {{<<"id">>,
                                        <<"platformer_node_5931B0A7C97D3F21A8AA6EAB5D2601A1">>},
                                       {<<"schem">>,<<"http">>},
                                       {<<"host">>,<<"0.0.0.0">>},
                                       {<<"port">>,8000},
                                       {<<"status">>,<<"active">>},
                                       {<<"rating">>,100},
                                       {<<"last_modified">>,1287423906298149.0}})).
