%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Server resource.
%%
%% This module handles creating and deleting server records in Platformer.
%%
%% Method      URI                      -->  Successful Response
%% GET         /server/list                  200 text/javascript list of servers (length of list is up to server)
%% POST        /server (+json-enc address)   201 Location: URI of new server ("/server/address_hash")
%% HEAD        /server/address_hash          200 (if server exists)
%% DELETE      /server/address_hash          204
%% OPTIONS     /server                       200 Access-Control-Allow-Methods: POST, OPTIONS
%% OPTIONS     /server/address_hash          200 Access-Control-Allow-Methods: HEAD, DELETE, OPTIONS

-module(server_resource).
-export([init/1, get_address/1, get_server/1, get_list/0, load_preconfigured/0, new_server/1, new_servers/1, to_json/2]).
-export([accept_content/2, allow_missing_post/2, allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2, create_path/2,
         delete_resource/2, delete_completed/2,
         malformed_request/2, moved_permanently/2,
         moved_temporarily/2, options/2,
         post_is_create/2, previously_existed/2,
         process_post/2,
         resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("external/jsonerl/jsonerl.hrl").

-include("platformer.hrl").

-record(context, {config,
                  body,     %% in a POST, this will hold the parsed body
                  record    %% in a GET, this will contain the retrieved server record
                 }).

%% This is used for ease in constructing a list of servers for a "/servers/list" query.
-record(servers, {servers}).

%% Webmachine functions

init(Config) ->
    {{trace, "/tmp"}, #context{config=Config}}.  %% debugging code
    %%{ok, #context{config=Config}}.             %% regular code

accept_content(ReqData, Context) ->
    {true, ReqData, Context}.

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['DELETE', 'GET', 'HEAD', 'OPTIONS', 'POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"text/javascript", accept_content}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/javascript", to_json}], ReqData, Context}.

create_path(ReqData, Context) ->
    case new_server(Context#context.record) of
        {ok, Server} ->
            Path = get_path(Server),
            {Path, wrq:set_resp_header("Location", Path, ReqData), Context#context{record=Server}};
        {error, Error} ->
            {"", wrq:append_to_response_body("Could not create new server.\n" ++ Error, ReqData), Context}
    end.

delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

delete_resource(ReqData, Context) ->
    {delete_server(wrq:path_info(hash, ReqData)), ReqData, Context}.

malformed_request(ReqData, Context) ->
    {MF, NewReqData, NewContext} =
        case wrq:method(ReqData) of
            'DELETE' ->
                case wrq:path_info(hash, ReqData) of
                    undefined -> 
                        {true, wrq:append_to_response_body("No server hash specified.", ReqData), Context};
                    _ ->
                        {false, ReqData, Context}
                end;
            'GET' ->
                {false, ReqData, Context};
            'HEAD' ->
                {case wrq:path_info(hash, ReqData) of undefined -> true; _ -> false end, ReqData, Context};
            'OPTIONS' ->
                {false, ReqData, Context};
            'POST' ->
                %% Check that the body contains a json object with the server spec
                Body = wrq:req_body(ReqData),
                {MF1, NRD1, NC1} =
                    try ?json_to_record(server, binary_to_list(Body)) of
                        %% Since we have to construct the record to determine its validity, we'll hang onto it
                        %% (We fix the scheme as an atom while we're at it.)
                        #server{} = Record -> 
                            {false, ReqData, Context#context{
                                               record = Record#server{scheme=binary_to_atom(Record#server.scheme, latin1)
                                                                           }}}
                    catch error ->
                            {true, wrq:append_to_response_body("Invalid or missing server specification."), Context}
                    end,
                %% Be sure the path is right.
                case wrq:path_info(hash, ReqData) of
                    undefined -> {false and MF1, NRD1, NC1};
                    _ -> {true, wrq:append_to_response_body("Do not POST to an existing server."), NC1}
                end
        end,
    {MF, pfr:postprocess_rd(NewReqData), NewContext}.

moved_permanently(ReqData, Context) ->
    {false, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    {false, ReqData, Context}.

options(ReqData, Context) ->
    {[{"Access-Control-Allow-Origin", "*"},
      case wrq:path_info(hash, ReqData) of
          undefined ->
              {"Access-Control-Allow-Methods", "OPTIONS, POST"};
          _ ->
              {"Access-Control-Allow-Methods", "HEAD, DELETE, OPTIONS"}
      end],
     ReqData, Context}.

%% POST is only create if the request body describes a server we don't yet know.
post_is_create(ReqData, Context) ->
    {case get_server(get_hash(Context#context.record)) of
         not_found ->
             log4erl:debug("Received POST with new server: ~p", [get_address(Context#context.record)]),
             true;
         _Server ->
             log4erl:debug("Received POST with previously known server: ~p", [get_address(Context#context.record)]),
             false
     end,
     ReqData, Context}.

previously_existed(ReqData, Context) ->
    Server = Context#context.record,
    {case Server#server.status of deleted -> true; _ -> false end,
     ReqData,
     Context}.

%% This will be called only if the server record has already been
%% found, in which case we just want to send a 303 "See Other"
%% response.
process_post(ReqData, Context) ->
    {true, wrq:do_redirect(true, wrq:set_resp_header("Location", get_path(Context#context.record), ReqData)), Context}.

resource_exists(ReqData, Context) ->
    resource_exists(wrq:method(ReqData), ReqData, Context).

resource_exists('GET', ReqData, Context) ->
    case wrq:raw_path(ReqData) of
        "/server/list" -> {true, ReqData, Context};
        _ -> server_exists(ReqData, Context)
    end;

resource_exists('DELETE', ReqData, Context) -> server_exists(ReqData, Context);
resource_exists('HEAD', ReqData, Context) -> server_exists(ReqData, Context);
resource_exists('POST', ReqData, Context) -> {false, ReqData, Context};
resource_exists(_, ReqData, Context) -> {true, ReqData, Context}.

server_exists(ReqData, Context) ->
    case get_server(list_to_binary(wrq:path_info('hash', ReqData))) of
        not_found -> {false, ReqData, Context};
        Server -> {true, ReqData, Context#context{record = Server}}
    end.

%% Resource-specific functions

%% @spec delete_userid(int()) -> bool()
delete_server(Hash) ->
    F = fun() ->
                [Server] = mnesia:read(user, Hash, write),
                mnesia:write(Server#server{status=deleted, last_modified=util:now_int()})
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            true;
        {aborted, _} ->
            false
    end.

new_servers([Server|Rest]) ->
    new_server(Server),
    new_servers(Rest);
new_servers([]) -> ok.

new_server(#server{} = Record) ->
    new_server(Record, 100);
new_server(Tuple) ->
    Proplist = [{binary_to_atom(X, latin1), Y} || {X, Y} <- tuple_to_list(Tuple)],
    RawRecord = list_to_tuple([server|[proplists:get_value(X, Proplist) || X <- record_info(fields, server)]]),
    Record = RawRecord#server{scheme=binary_to_atom(RawRecord#server.scheme, latin1)},
    new_server(Record).

new_server(#server{} = Record, Rating) ->
    case is_me(Record) of
        true ->
            log4erl:info("Will not create record for own node."),
            is_me;
        _ ->
            Address = get_address(Record),
            Hash = get_hash(Address),

            %% First check whether server record already exists in database.
            case get_server(Hash) of
                not_found ->
                                                % Augment the supplied record to become a full-fledged server record.
                    Server = Record#server{status=active,
                                           hash=list_to_binary(Hash),
                                           rating=Rating,
                                           last_modified=util:now_int()},
                    log4erl:info("Creating new server with address ~p.", [Address]),
                    case platformer_db:write(Server) of
                        {atomic, ok} ->
                            {ok, Server};
                        {aborted, Error} ->
                            {error, Error}
                    end;
                Server ->
                    log4erl:info("Server ~p is already known.", [Address]),
                    {already_exists, Server}
            end
    end.

get_address(#server{scheme=Scheme, host=Host, port=Port}) ->
    lists:concat([Scheme, "://", binary_to_list(Host), ":", Port]).

get_hash(#server{} = Record) ->
    get_hash(get_address(Record));
get_hash(Address) ->
    util:md5(Address).

get_path(#server{hash = Hash}) ->
     "/server/" ++ binary_to_list(Hash).

%% Is the given server "me" (the current node)?
is_me(#server{host=Host, port=Port}) ->
    case binary_to_list(Host) =:= util:get_param(ip) of
        true -> Port =:= util:get_param(port);
        false -> false
    end.

%% @spec to_json(rd(), term()) -> {string(), rd(), term()}
to_json(ReqData, Context) ->
    case wrq:method(ReqData) of
        'GET' ->
            case wrq:path_info(hash, ReqData) of
                undefined ->
                    %% Of the known servers with rating 75 or greater,
                    %% share a random sample of 25%.
                    {case platformer_db:find(qlc:q([X || X <- mnesia:table(server), X#server.rating > 75])) of
                         undefined -> <<>>;
                         [] -> <<"[]">>;
                         Servers ->
                             Choices = lists:sublist(util:shuffle(Servers), trunc(length(Servers) * 0.25 + 1)),
                             ListRecord = #servers{servers=[?record_to_struct(server, Server) || Server <- Choices]},
                             jsonerl:encode(?record_to_struct(servers, ListRecord))
                     end,
                     ReqData, Context};
                Hash ->
                    {case platformer_db:find(qlc:q([X || X <- mnesia:table(server), X#server.hash == Hash])) of
                         undefined -> <<>>;
                         Server -> ?record_to_json(server, Server)
                     end,
                     ReqData, Context}
            end;
        _ ->
            {<<>>, ReqData, Context}
    end.

%% Return all servers.
get_list() ->
    platformer_db:read_all(server).

get_server(Hash) ->
    Result = platformer_db:find(qlc:q([X || X <- mnesia:table(server), X#server.hash == list_to_binary(Hash)])),
    case length(Result) of
        1 ->
            hd(Result);
        0 ->
            not_found
    end.
    
%% Ensure any preconfigured servers are in the database.
load_preconfigured() ->
    case application:get_env(platformer, servers) of
        {ok, Servers} ->
            log4erl:info("Loading ~B preconfigured server(s) from app config.", [length(Servers)]),
            load_preconfigured(Servers);
        _ ->
            ok
    end.

load_preconfigured([Address|Rest]) ->
    log4erl:debug("Preconfigured server: ~p", [Address]),
    {Scheme, _UserInfo, Host, Port, _Path, _Query} = http_uri:parse(Address),
    new_server(#server{scheme=Scheme, host=list_to_binary(Host), port=Port}),
    load_preconfigured(Rest);
load_preconfigured([]) -> ok.
