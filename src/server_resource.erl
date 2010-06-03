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
-export([init/1, get_servers/0, load_preconfigured/0, new_server/1, new_server/2, server_exists/1, to_json/2]).
-export([allow_missing_post/2, allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2, create_path/2,
         delete_resource/2, delete_completed/2,
         malformed_request/2, moved_permanently/2,
         moved_temporarily/2, options/2,
         post_is_create/2, previously_existed/2,
         resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("platformer.hrl").

-record(context, {config, address, status, hash, last_modified}).

%% Webmachine functions

init(Config) ->
    {{trace, "/tmp"}, #context{config=Config}}.  %% debugging code
    %%{ok, #context{config=Config}}.             %% regular code

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['DELETE', 'GET', 'HEAD', 'OPTIONS', 'POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"text/javascript", accept_content}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/javascript", to_json}], ReqData, Context}.

create_path(ReqData, Context) ->
    {Hash, Path} = new_server(Context#context.address),
    {Path, wrq:set_resp_header("Location", Path, ReqData), Context#context{hash=Hash}}.

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
                        {true,
                         wrq:append_to_response_body("No server hash specified.", ReqData),
                         Context};
                    _ ->
                        {false, ReqData, Context}
                end;
            'GET' ->
                {wrq:path(ReqData) =/= "/server/list",
                 ReqData,
                 Context};
            'HEAD' ->
                {case wrq:path_info(hash, ReqData) of undefined -> true; _ -> false end,
                 ReqData,
                 Context};
            'OPTIONS' ->
                {false, ReqData, Context};
            'POST' ->
                %% Check that the body contains a json object with the address
                Body = wrq:req_body(ReqData),
                {struct, [{"address", Address}]} =
                    try mochijson:decode(Body)
                    catch
                        error:X -> {Body, caught, error, X}
                    end,
                %% Be sure the path is right.
                {case wrq:path_info(hash, ReqData) of undefined -> false; _ -> true end,
                 ReqData,
                 Context#context{address=Address}}
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

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

previously_existed(ReqData, Context) ->
    {case Context#context.status of deleted -> true; _ -> false end,
     ReqData,
     Context}.

resource_exists(ReqData, Context) ->
    {Exists, NewContext} =
        case wrq:method(ReqData) of
            Method when Method =:= 'DELETE' orelse Method =:= 'HEAD' ->
                {Found, Status} = server_exists(wrq:path_info('hash', ReqData)),
                {Found, Context#context{status = Status}};
            'POST' ->
                {false, Context};
            _ ->
                {true, Context}
        end,
    {Exists, ReqData, NewContext}.

%% Resource-specific functions

%% @spec delete_userid(int()) -> bool()
delete_server(Hash) ->
    F = fun() ->
                [Server] = mnesia:read(user, Hash, write),
                mnesia:write(Server#server{status=deleted, last_modified=now()})
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            true;
        {aborted, _} ->
            false
    end.

%% @spec new_server(string()) -> {string(), string()}
new_server(Address) ->
    new_server(Address, 100).

%% @spec new_server(string(), int()) -> {string(), string()}
new_server(Address, Rating) ->
    Hash = util:md5(Address),
    case platformer_db:write(#server{address=Address, status=active, hash=Hash, rating=Rating, last_modified=now()}) of
        {atomic, ok} ->
            {Hash, string:concat("/server/", Hash)};
        {aborted, Error} ->
            throw(Error)
    end.

%% @spec to_json(rd(), term()) -> {string(), rd(), term()}
to_json(ReqData, Context) ->
    case wrq:method(ReqData) of
        'GET' ->
            %% Of the known servers with rating 75 or greater,
            %% share a random sample of 25%.
            Servers = platformer_db:find(qlc:q([X || X <- mnesia:table(server),
                                                     X#server.rating > 75])),
            Choices = lists:sublist(util:shuffle(Servers), trunc(length(Servers) * 0.25 + 1)),
            {json:ify([{servers, {array, [{struct, [{address, Server#server.address}]} || Server <- Choices]}}]),
             ReqData, Context};
        'HEAD' ->
            {json:ify([{address, Context#context.address}]), ReqData, Context};
        _ ->
            {<<>>, ReqData, Context}
    end.

get_servers() ->
    platformer_db:read_all(server).

%% @spec server_exists(int()) -> bool()
server_exists(Hash) ->
    Result = platformer_db:find(qlc:q([X || X <- mnesia:table(server),
                                            X#server.hash == Hash])),
    case length(Result) of
        1 ->
            Server = hd(Result),
            case Server#server.status of
                active -> {true, active};
                deleted -> {false, deleted}
            end;
        0 ->
            {false, unknown};
        _ ->
            throw({codingError, "Multiple results found for hash!"})
    end.
    
%% Ensure any preconfigured servers are in the database.
load_preconfigured() ->        
    case application:get_env(platformer, servers) of
        {ok, Servers} ->
            [server_resource:new_server(Address) || Address <- Servers];
        _ ->
            true
    end.
