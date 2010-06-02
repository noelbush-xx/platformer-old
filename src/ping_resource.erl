%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc A very simple resource to respond to pings.
-module(ping_resource).
-export([init/1, allowed_methods/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
    {{trace, "/tmp"}, Config}.  %% debugging code
    %%{ok, Config}.             %% regular code

allowed_methods(ReqData, Context) ->
    {['OPTIONS'], ReqData, Context}.
