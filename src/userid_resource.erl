%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.

-module(userid_resource).
-export([init/1, to_json/2]).
-export([allowed_methods/2, content_types_provided/2, delete_resource/2, delete_completed/2, malformed_request/2, options/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("records.hrl").

%% Webmachine functions

init(Config) ->
    {{trace, "/tmp"}, Config}.  %% debugging code
    %%{ok, Config}.             %% regular code

allowed_methods(ReqData, Context) ->
    {['GET', 'DELETE', 'OPTIONS'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/javascript", to_json}], ReqData, Context}.

delete_resource(ReqData, Context) ->
    {delete_userid(wrq:path_info(id, ReqData)), ReqData, Context}.

delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

malformed_request(ReqData, Context) ->
    MF = case wrq:method(ReqData) of
             'GET' ->
                 case wrq:get_qs_value("callback", ReqData) of
                     'undefined' -> true;
                     _ -> false
                 end;
             'DELETE' ->
                 case wrq:path_info(id, ReqData) of
                     'undefined' -> true;
                     _ -> false
                 end;
             'OPTIONS' ->
                 false;
             _ ->
                 true
         end,
    {MF, ReqData, Context}.

options(ReqData, Context) ->
    {[{"Access-Control-Allow-Origin", "*"},
      {"Access-Control-Allow-Methods", "GET, DELETE, OPTIONS"}
     ], ReqData, Context}.

%% Resource-specific functions

delete_userid(Id) ->
    case platformer_db:delete({userid, Id}) of
        {atomic, _Result} ->
            true;
        _ ->
            false
    end.

new_userid() ->
    Id = string:concat("platformer_user_", uuid:to_string(uuid:v4())),
    platformer_db:write(#userid{userid=Id, created=now()}),
    Id.

to_json(ReqData, State) ->
    {jsonp:wrap(wrq:get_qs_value("callback", ReqData), [{"userid", list_to_binary(new_userid())}]), ReqData, State}.
