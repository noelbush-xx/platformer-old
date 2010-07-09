%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc A very simple resource to respond to pings.
-module(platformer.webmachine.ping_resource).
-export([init/1, allowed_methods/2, options/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
    %%{{trace, "/tmp/platformer/" ++ atom_to_list(node())}, Config}.  %% debugging code
    {ok, Config}.             %% regular code

allowed_methods(ReqData, Context) ->
    {['OPTIONS'], ReqData, Context}.

options(ReqData, Context) ->
    {[{"Access-Control-Allow-Origin", "*"}, {"Access-Control-Allow-Methods", "OPTIONS"}], ReqData, Context}.
