-module(platformer.core.launcher).

-export([startup/0]).

-import(application).
-import(code).
-import(crone).
-import(file).
-import(filelib).
-import(filename).
-import(init).
-import(io).
-import(lists).
-import(log4erl).
-import(os).
-import(proplists).
-import(re).
-import(string).

-import(platformer.core.liaison).
-import(platformer.core.util).

startup() ->
    %% Read config values from a(n optionally supplied) config file
    Ip = util:get_param(ip, case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end),
    Port = util:get_param(port, 8000),
    PrivDir = filename:join([filename:dirname(code:which(?MODULE)), "..", "..", "..", "priv"]),
    DispatchPath = filename:join([PrivDir, util:get_param(dispatch, "dispatch.conf")]),
    WMLogDir = filename:join([PrivDir, util:get_param(log_dir, "log")]),
    SeparateLog = util:get_param(separate_log, true),

    %% Start and configure logging.
    application:start(log4erl),
    log4erl:conf(filename:join([PrivDir, "log4erl.conf"])),
    NodeFullName = atom_to_list(node()),
    NodeShortName = string:substr(NodeFullName, 1, string:chr(NodeFullName, $@) - 1),
    case log4erl:change_format(file, "[" ++ NodeShortName  ++ "][%L] %j %t %l%n") of
        {error, E0} -> io:format("*** Error changing logger format: ~p~n", [E0]);
        ok -> ok
    end,

    %% If configured to do so, modify the logger to include the node name.
    case SeparateLog of
        true ->
            case log4erl:change_filename(file, NodeShortName) of
                {error, E1} -> io:format("*** Error changing logger filename to ~p. (~p)~n", [NodeShortName, E1]);
                ok -> ok
            end;
        _ -> ok
    end,

    %% Write the pid to a file
    PidFile = filename:join([PrivDir, "pid", lists:concat([string:sub_word(atom_to_list(node()), 1, $@), ".pid"])]),
    file:write_file(PidFile, os:getpid()),

    %% Load the webmachine dispatch config.
    DispatchContent =
        try file:consult(DispatchPath) of
            {ok, Content} -> Content
        catch error:E2 ->
                log4erl:error("Error reading dispatch file at " ++ DispatchPath, E2),
                exit("Could not read dispatch file.")
        end,

    %% Special handling of the wmtrace directory path (the token
    %% "%node%" will be replaced with the current node name, and the
    %% directory will be created if it does not exist).
    Dispatch = lists:map(fun({Path, wmtrace_resource, [{trace_dir, InitialTraceDir}]}) ->
                                 {Path, wmtrace_resource, [{trace_dir, re:replace(InitialTraceDir, "%node%", atom_to_list(node()), [{return, list}])}]};
                            (Term) -> Term
                         end,
                         DispatchContent),
    TraceDir = proplists:get_value(trace_dir, lists:nth(3, tuple_to_list(lists:keyfind(wmtrace_resource, 2, Dispatch)))),
    case filelib:ensure_dir(filename:join(TraceDir, "example")) of
        ok -> ok;
        {error, Reason} ->
            log4erl:error("Could not find or create wmtrace directory ~s (Reason: ~s).~n", [TraceDir, Reason]),
            exit("Invalid wmtrace directory specified. " ++ Reason)
    end,

    %% Prepare the configuration for webmachine/mochiweb.
    WebConfig = [{ip, Ip},
                 {port, Port},
                 {log_dir, WMLogDir},
                 {dispatch, Dispatch},
                 {error_handler, platformer.webmachine.error_handler}],

    Web = {webmachine_mochiweb, {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    %% Check that the database is set up (dev purposes only -- later need real release management).
    platformer.core.db:check_tables(),

    %% Reset the db if instructed to; in any case, check that pre-supplied servers are in db
    case lists:member("reset-db", init:get_plain_arguments()) of
        true -> platformer.core.db:reset();
        false -> platformer.core.node:load_preconfigured()
    end,

    %% Announce self to other servers, seek peers, and set up timed announcements and peer searches.
    liaison:announce_self(),
    liaison:seek_peers(),
    crone:start([{{daily,{every,{5,min},{between,{12,am},{11,55,pm}}}},
                  {platformer.core.liaison,announce_self,[]}}]),
    crone:start([{{daily,{every,{5,min},{between,{12,am},{11,55,pm}}}},
                  {platformer.core.liaison,seek_peers,[]}}]),
    
                     
    log4erl:info("Starting up Platformer node listening on port ~B.", [Port]),

    {ok, {{one_for_one, 10, 10}, [Web]}}.
