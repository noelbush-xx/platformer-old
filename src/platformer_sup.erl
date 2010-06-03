%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.

%% @doc Supervisor for the platformer application.

-module(platformer_sup).
-author('Noel Bush <noel@platformer.org>').

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
    %% Read config values from a(n optionally supplied) config file
    Ip = util:get_param(ip, case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end),
    Ports = util:get_param(ports, [8000]),
    PrivDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]),
    DispatchPath = util:get_param(dispatch, filename:join([PrivDir, "dispatch.conf"])),
    LogDir = util:get_param(log_dir, filename:join(PrivDir, "log")),

    Dispatch =
        case file:consult(DispatchPath) of
            {ok, File} -> File;
            {error, Error} -> throw({error, "Error reading dispatch file at " ++ DispatchPath, Error})
        end,

    BaseConfig = [{ip, Ip},
                  {log_dir, LogDir},
                  {dispatch, Dispatch},
                  {error_handler, platformer_error_handler}],

    Processes = [config_process(BaseConfig, Port) || Port <- Ports],

    %%server_resource:load_preconfigured(),

    {ok, {{one_for_one, 10, 10}, Processes}}.

config_process(BaseConfig, Port) ->
    ChildName = list_to_atom(lists:concat(["platformer_webmachine_", Port])),
    WebConfig = [{child_name, ChildName}|[{port, Port}|BaseConfig]],
    {ChildName,
     {webmachine_mochiweb, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
