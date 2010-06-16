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
    WMLogDir = filename:join([PrivDir, util:get_param(log_dir, "log")]),

    %% Configure Platformer logging.
    application:start(log4erl),
    log4erl:conf(filename:join([PrivDir, "log4erl.conf"])),

    %% Write the pid to a file
    PidFile = filename:join([PrivDir, lists:concat([string:sub_word(atom_to_list(node()), 1, $@), ".pid"])]),
    file:write_file(PidFile, os:getpid()),

    %% Configure the logger to include the node name.
    NodeFullName = atom_to_list(node()),
    NodeShortName = string:substr(NodeFullName, 1, string:chr(NodeFullName, $@) - 1),
    case log4erl:change_format(file, "[" ++ NodeShortName  ++ "][%L] %j %t %l%n") of
        {error, E0} -> io:format("*** Error changing logger format: ~p~n", [E0]);
        ok -> ok
    end,
    case log4erl:change_filename(file, NodeShortName) of
        {error, E1} -> io:format("*** Error changing logger filename to ~p. (~p)~n", [NodeShortName, E1]);
        ok -> ok
    end,

    %% Load the webmachine dispatch config.
    Dispatch =
        case file:consult(DispatchPath) of
            {ok, File} -> File;
            {error, E2} -> log4erl:error("Error reading dispatch file at " ++ DispatchPath, E2)
        end,

    %% Prepare the configuration for webmachine/mochiweb.
    WebConfig = [{ip, Ip},
                 {port, Port},
                 {log_dir, WMLogDir},
                 {dispatch, Dispatch},
                 {error_handler, platformer_error_handler}],

    Web = {webmachine_mochiweb, {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    %% Check that the database is set up (dev purposes only -- later need real release management).
    platformer_db:check_tables(),

    %% Reset the db if instructed to; in any case, check that pre-supplied servers are in db
    case lists:member("reset-db", init:get_plain_arguments()) of
        true -> platformer_db:reset();
        false -> pfnode:load_preconfigured()
    end,

    %% Announce self to other servers, seek peers, and set up timed announcements and peer searches.
    liaison:announce_self(),
    liaison:seek_peers(),
    crone:start([{{daily,{every,{5,min},{between,{12,am},{11,55,pm}}}},
                  {liaison,announce_self,[]}}]),
    crone:start([{{daily,{every,{5,min},{between,{12,am},{11,55,pm}}}},
                  {liaison,seek_peers,[]}}]),
    
                     
    log4erl:info("Starting up Platformer node listening on port ~B.", [Port]),

    {ok, {{one_for_one, 10, 10}, [Web]}}.
