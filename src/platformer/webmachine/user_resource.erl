%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc User resource.
%%
%% This resource represents a user in Platformer space.
%%
%% <table class="resource">
%% <tr><th>Method</th>     <th>URI</th>                          <th>Successful Response</th></tr>
%% <tr><td>POST</td>        <td>/user</td>                       <td>201 Content-Type: text/javascript<br/>
%%                                                                   Location: URI of new userid<br/>
%%                                                                   {"userid":new_user};<br/></td></tr>
%% <tr><td>HEAD</td>        <td>/user/some_userid</td>           <td>200 (if id exists)</td></tr>
%% <tr><td>DELETE</td>      <td>/user/some_userid</td>           <td>204</td></tr>
%% <tr><td>OPTIONS</td>     <td>/user</td>                       <td>200 Access-Control-Allow-Methods: POST, OPTIONS</td></tr>
%% <tr><td>OPTIONS</td>     <td>/user/some_userid</td>           <td>200 Access-Control-Allow-Methods: HEAD, DELETE, OPTIONS</td></tr>
%% </table>

-module(platformer.webmachine.user_resource).
-export([init/1, to_json/2]).
-export([allow_missing_post/2, allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2, create_path/2,
         delete_resource/2, delete_completed/2,
         malformed_request/2, moved_permanently/2,
         moved_temporarily/2, options/2,
         post_is_create/2, previously_existed/2,
         resource_exists/2]).

-import(jsonerl).
-import(lists).
-import(wrq).

-import(platformer.core.user).

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("platformer.hrl").

-record(context, {config, userid, path, status}).

%% Webmachine functions

%% @doc See {@wmdocs}
init(Config) ->
    {{trace, "/tmp/platformer/" ++ atom_to_list(node())}, #context{config=Config}}.  %% debugging code
    %%{ok, #context{config=Config}}.                                                 %% regular code

%% @doc See {@wmdocs}
allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc See {@wmdocs}
allowed_methods(ReqData, Context) ->
    {['DELETE', 'HEAD', 'OPTIONS', 'POST'], ReqData, Context}.

%% @doc See {@wmdocs}
content_types_accepted(ReqData, Context) ->
    {[{"application/x-www-form-urlencoded", to_json},  %% default as sent by jquery & forms in general
      {"application/octet-stream", to_json}            %% default set by webmachine when CT isn't supplied
     ], ReqData, Context}.

%% @doc See {@wmdocs}
content_types_provided(ReqData, Context) ->
    {[{"text/javascript", to_json}], ReqData, Context}.

%% @doc See {@wmdocs}
create_path(ReqData, Context) ->
    {Id, Path} = user:create(),
    {Path, wrq:set_resp_header("Location", Path, ReqData), Context#context{userid=Id, path=Path}}.

%% @doc See {@wmdocs}
delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc See {@wmdocs}
delete_resource(ReqData, Context) ->
    {user:delete(list_to_binary(wrq:path_info(id, ReqData))), ReqData, Context}.

%% @doc See {@wmdocs}
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
                {wrq:path_info(id, ReqData) == undefined, ReqData, Context};
            'OPTIONS' ->
                {false, ReqData, Context};
            'POST' ->
                {wrq:path_info(id, ReqData) =/= undefined, ReqData, Context}
        end,
    {MF, common:postprocess_rd(NewReqData), NewContext}.

%% @doc See {@wmdocs}
moved_permanently(ReqData, Context) ->
    {false, ReqData, Context}.

%% @doc See {@wmdocs}
moved_temporarily(ReqData, Context) ->
    {false, ReqData, Context}.

%% @doc See {@wmdocs}
options(ReqData, Context) ->
    {lists:flatten(
       [{"Access-Control-Allow-Origin", "*"},
        case wrq:path_info(id, ReqData) of
            undefined ->
                {"Access-Control-Allow-Methods", "OPTIONS, POST"};
            _ ->
                [{"Access-Control-Allow-Methods", "HEAD, DELETE, OPTIONS"},
                 {"Access-Control-Allow-Headers", "X-Platformer-Query-Token, X-Platformer-Query-Age"}]
        end]),
     ReqData, Context}.

%% @doc See {@wmdocs}
post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc See {@wmdocs}
previously_existed(ReqData, Context) ->
    {case Context#context.status of deleted -> true; _ -> false end,
     ReqData,
     Context}.

%% @doc See {@wmdocs}
resource_exists(ReqData, Context) ->
    {Exists, NewContext} =
        case wrq:method(ReqData) of
            Method when Method =:= 'DELETE' orelse Method =:= 'HEAD' ->
                {Found, Status} = 
                    %% Only check other servers if the X-Platformer-Query-Token and
                    %% X-Platformer-Query-Age headers are present.
                    user:exists({list_to_binary(wrq:path_info('id', ReqData)), common:valid_extended_query_request(ReqData)}),
                {Found, Context#context{status=Status}};
            'POST' ->
                {false, Context};
            _ ->
                {true, Context}
        end,
    {Exists, ReqData, NewContext}.

%% @spec to_json(wm_reqdata(), term()) -> {string(), rd(), term()}
to_json(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {true,
             wrq:set_resp_body(jsonerl:encode({{userid, Context#context.userid}}), ReqData),
             Context};
        _ ->
            {<<>>, ReqData, Context}
    end.

