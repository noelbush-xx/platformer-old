%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.
%%
%% This module handles creating and deleting users in Platformer.
%% A standard REST interface is augmented with a GET+jsonp method
%% to accomodate web browsers with stringent cross-domain security models.
%%
%% Method      URI                      -->  Successful Response
%% POST        /userid                       201 Content-Type: application/json
%%                                               Location: URI of new userid
%%                                               {"userid":new_userid}
%% GET         /userid?callback=abcxyz       201 Content-Type: application/json
%%                                               Location: URI of new userid
%%                                               abcxyz({"userid":new_userid})
%% GET         /userid/some_userid           200 (if id exists)
%% DELETE      /userid/some_userid           204
%% OPTIONS     /userid                       200 Access-Control-Allow-Methods: GET, POST, OPTIONS
%% OPTIONS     /userid/some_userid           200 Access-Control-Allow-Methods: GET, DELETE, OPTIONS

-module(userid_resource).
-export([init/1, to_json/2]).
-export([allow_missing_post/2, allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2, create_path/2,
         delete_resource/2, delete_completed/2,
         malformed_request/2, options/2,
         post_is_create/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").

-record(context, {config, callback, userid, path}).

%% Webmachine functions

init(Config) ->
    {{trace, "/tmp"}, #context{config=Config}}.  %% debugging code
    %%{ok, #context{config=Config}}.             %% regular code

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET', 'DELETE', 'OPTIONS', 'POST'], ReqData, Context}.

create_path(ReqData, Context) ->
    {Id, Path} = new_userid(),
    {Path, ReqData, Context#context{userid=Id, path=Path}}.

content_types_accepted(ReqData, Context) ->
    {[{"application/x-www-form-urlencoded", to_json},  %% default as sent by jquery & forms in general
      {"application/octet-stream", to_json}            %% default set by webmachine when CT isn't supplied
     ], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

delete_resource(ReqData, Context) ->
    {delete_userid(wrq:path_info(id, ReqData)), ReqData, Context}.

delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

%% This does something a little unusual -- it checks if we're getting
%% a GET + jsonp callback request, and if so changes it to a POST
%% (preserving the callback).  This allows us to use the same processing
%% path for what is really the same request (GET+jsonp is just a browser
%% security option that doesn't really help us here).
malformed_request(ReqData, Context) ->
    case wrq:method(ReqData) of
        'GET' ->
            case wrq:get_qs_value("callback", ReqData) of
                undefined ->
                    {true,
                     wrq:append_to_response_body("No callback specified.", ReqData),
                     Context};
                Callback ->
                    {false,
                     ReqData#wm_reqdata{method='POST'},
                     Context#context{callback=Callback}}
            end;
        'DELETE' ->
            case wrq:path_info(id, ReqData) of
                undefined -> 
                    {true,
                     wrq:append_to_response_body("No userid specified."),
                     Context};
                _ ->
                    {false, ReqData, Context}
            end;
        _ ->
            {false, ReqData, Context}
    end.

options(ReqData, Context) ->
    {[{"Access-Control-Allow-Origin", "*"},
      {"Access-Control-Allow-Methods", "GET, DELETE, OPTIONS"}
     ], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

resource_exists(ReqData, Context) ->
    Exists =
        case wrq:method(ReqData) of
            'GET' ->
                case wrq:path_info('id', ReqData) of
                    undefined -> true;
                    RequestedId ->
                        userid_exists(RequestedId)
                end;
            'POST' ->
                false;
            _ ->
                true
        end,
    {Exists, ReqData, Context}.

%% Resource-specific functions

%% @spec delete_userid(int()) -> bool()
delete_userid(Id) ->
    case platformer_db:delete({user, Id}) of
        {atomic, _Result} ->
            true;
        _ ->
            false
    end.

%% @spec new_userid() -> {Id, Path}
new_userid() ->
    Id = string:concat("platformer_user_", uuid:to_string(uuid:v4())),
    platformer_db:write(#user{id=Id, created=now()}),
    {Id, string:concat("/userid/", Id)}.

%% @spec to_json(rd(), term()) -> {string(), rd(), term()}
%%
%% Here we make sure to wrap the result in a callback (meaning that
%% we originally got this from a browser as a GET+jsonp request).
to_json(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {Id, Path} = {Context#context.userid, Context#context.path},
            Body =
                case Context#context.callback of
                    undefined ->
                        json:ify([{userid, Id}]);
                    Callback ->
                        json:wrap(Callback, [{userid, Id}])
                end,
            {true,
             wrq:set_resp_header("Location", Path,
                                 wrq:set_resp_body(Body, ReqData)),
             Context};
        _ ->
            {"", ReqData, Context}
    end.

%% @spec userid_exists(int()) -> bool()
userid_exists(Id) ->
    length(platformer_db:find(qlc:q([X || X <- mnesia:table(user),
                                           X#user.id == Id]))) > 0.
