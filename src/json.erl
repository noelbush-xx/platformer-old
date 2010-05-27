%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc json and jsonp utilities.
-module(json).

-export([ify/1, wrap/2]).

%% Encode the given data as json and wrap it with the given prefix.
wrap(Prefix, Data) ->
  io_lib:format("~s(~s)", [Prefix, json:ify(Data)]).

%% json-ify the given data.
ify(Data) ->
  mochijson:encode({struct, Data}).
    
