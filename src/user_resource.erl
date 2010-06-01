%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.
%%
%% This module handles creating and deleting users in Platformer.
%%
%% Method      URI                    -->  Successful Response
%% POST        /user                       201 Content-Type: text/javascript
%%                                               Location: URI of new userid
%%                                               {"userid":new_userid};
%% HEAD        /user/some_userid           200 (if id exists)
%% DELETE      /user/some_userid           204
%% OPTIONS     /user                       200 Access-Control-Allow-Methods: POST, OPTIONS
%% OPTIONS     /user/some_userid           200 Access-Control-Allow-Methods: HEAD, DELETE, OPTIONS

-module(user_resource).
-export([init/1, to_json/2]).
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

-record(context, {config, userid, path, status}).

%% Webmachine functions

init(Config) ->
    {{trace, "/tmp"}, #context{config=Config}}.  %% debugging code
    %%{ok, #context{config=Config}}.             %% regular code

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['DELETE', 'HEAD', 'OPTIONS', 'POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/x-www-form-urlencoded", to_json},  %% default as sent by jquery & forms in general
      {"application/octet-stream", to_json}            %% default set by webmachine when CT isn't supplied
     ], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/javascript", to_json}], ReqData, Context}.

create_path(ReqData, Context) ->
    {Id, Path} = new_userid(),
    {Path, wrq:set_resp_header("Location", Path, ReqData), Context#context{userid=Id, path=Path}}.

delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

delete_resource(ReqData, Context) ->
    {delete_userid(wrq:path_info(id, ReqData)), ReqData, Context}.

malformed_request(ReqData, Context) ->
    {MF, NewReqData, NewContext} =
        case wrq:method(ReqData) of
            'DELETE' ->
                case wrq:path_info(id, ReqData) of
                    undefined -> 
                        {true,
                         wrq:append_to_response_body("No userid specified.", ReqData),
                         Context};
                    _ ->
                        {false, ReqData, Context}
                end;
            'HEAD' ->
                {case wrq:path_info(id, ReqData) of undefined -> true; _ -> false end,
                 ReqData,
                 Context};
            'OPTIONS' ->
                {false, ReqData, Context};
            'POST' ->
                {case wrq:path_info(id, ReqData) of undefined -> false; _ -> true end,
                 ReqData,
                 Context}
        end,
    {MF, wrq:set_resp_header("Access-Control-Allow-Origin", "*", NewReqData), NewContext}.

moved_permanently(ReqData, Context) ->
    {false, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    {false, ReqData, Context}.

options(ReqData, Context) ->
    {[{"Access-Control-Allow-Origin", "*"},
      case wrq:path_info(id, ReqData) of
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
                {Found, Status} = user_exists(wrq:path_info('id', ReqData)),
                {Found, Context#context{status=Status}};
            'POST' ->
                {false, Context};
            _ ->
                {true, Context}
        end,
    {Exists, ReqData, NewContext}.

%% Resource-specific functions

%% @spec delete_userid(int()) -> bool()
delete_userid(Id) ->
    F = fun() ->
                [User] = mnesia:read(user, Id, write),
                mnesia:write(User#user{status=deleted, last_modified=now()})
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            true;
        {aborted, _} ->
            false
    end.

%% @spec new_userid() -> {Id, Path}
new_userid() ->
    Id = string:concat("platformer_user_", uuid:to_string(uuid:v4())),
    case platformer_db:write(#user{id=Id, status=active, last_modified=now()}) of
        {atomic, ok} ->
            {Id, string:concat("/userid/", Id)};
        {aborted, Error} ->
            throw(Error)
    end.

%% @spec to_json(rd(), term()) -> {string(), rd(), term()}
to_json(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {true,
             wrq:set_resp_body(json:ify([{userid, Context#context.userid}]), ReqData),
             Context};
        _ ->
            {<<>>, ReqData, Context}
    end.

%% @spec user_exists(int()) -> {bool(), atom()}
user_exists(Id) ->
    %% First check the local database.
    Result = platformer_db:find(qlc:q([X || X <- mnesia:table(user),
                                            X#user.id == Id])),
    case length(Result) of
        1 ->
            User = hd(Result),
            case User#user.status of
                active -> {true, active};
                deleted -> {false, deleted}
            end;
        0 ->
            %% TODO: Go check with other servers
            user_exists(Id, server_resource:get_servers());
        _ ->
            throw({codingError, "Multiple results found for id!"})
    end.
    
user_exists(Id, [Server|Servers]) ->
    case httpc:request(head, {Server#server.address ++ "/user/" ++ Id, []}, [], []) of
        {ok, {{_, Status, _}, _, _}} ->
            case Status of
                200 -> {true, active};
                410 -> {false, deleted};
                _ -> user_exists(Id, Servers)
            end;
        {error, _Reason} -> user_exists(Id, Servers)
end;
user_exists(_Id, []) -> {false, unknown}.
