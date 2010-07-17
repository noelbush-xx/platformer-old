%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

%% @doc platformer startup code

-module(platformer).
-author('Noel Bush <noel@platformer.org>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    setup(),
    platformer.otp.sup:start_link().

%% @spec start() -> ok
%% @doc Start the platformer server.
start() ->
    setup(),
    application:start(platformer).

setup() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(mnesia),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine).

%% @spec stop() -> ok
%% @doc Stop the platformer server.
stop() ->
    Res = application:stop(platformer),
    application:stop(inets),
    application:stop(mnesia),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    Res.
