%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

%% @doc Supervisor for the platformer application.

-module(platformer_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = util:get_param(ip, "0.0.0.0"),
    Port = util:get_param(port, 8000),
    DispatchPath = util:get_param(dispatch, "priv/dispatch.conf"),
    LogDir = util:get_param(log_dir, "priv/log"),
    
    {ok, Dispatch} = file:consult(DispatchPath),

    Config = [{ip, Ip},
              {port, Port},
              {log_dir, LogDir},
              {dispatch, Dispatch},
              {error_handler, platformer_error_handler}],

    Web = {webmachine_mochiweb,
	   {webmachine_mochiweb, start, [Config]},
	   permanent, 5000, worker, dynamic},
    
    server_resource:load_preconfigured(),

    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.
