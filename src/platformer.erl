%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

%% @doc TEMPLATE.

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
    platformer_deps:ensure(),
    ensure_started(crypto),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    platformer_sup:start_link().

%% @spec start() -> ok
%% @doc Start the platformer server.
start() ->
    platformer_deps:ensure(),
    ensure_started(crypto),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(platformer).

%% @spec stop() -> ok
%% @doc Stop the platformer server.
stop() ->
    Res = application:stop(platformer),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
