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
    Port = util:get_param(port, 8000),
    PrivDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]),
    DispatchPath = filename:join([PrivDir, util:get_param(dispatch, "dispatch.conf")]),
    LogDir = filename:join([PrivDir, util:get_param(log_dir, "log")]),

    Dispatch =
        case file:consult(DispatchPath) of
            {ok, File} -> File;
            {error, Error} -> throw({error, "Error reading dispatch file at " ++ DispatchPath, Error})
        end,

    WebConfig = [{ip, Ip},
                 {port, Port},
                 {log_dir, LogDir},
                 {dispatch, Dispatch},
                 {error_handler, platformer_error_handler}],
io:format("webconfig: ~p~n", [WebConfig]),
    Web = {webmachine_mochiweb, {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    %% Reset the db if instructed to; in any case, check that pre-supplied servers are in db
    %% case lists:member("reset-db", init:get_plain_arguments()) of
    %%     true -> platformer_db:reset();
    %%     false -> server_resource:load_preconfigured()
    %% end,

    %% Write the pid to a file
    PidFile = filename:join([PrivDir, lists:concat([string:sub_word(atom_to_list(node()), 1, $@), ".pid"])]),
    file:write_file(PidFile, os:getpid()),

    {ok, {{one_for_one, 10, 10}, [Web]}}.
