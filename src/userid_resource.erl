%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.

-module(userid_resource).
-export([init/1, to_json/2]).
-export([allow_missing_post/2, allowed_methods/2,
         content_types_accepted/2, content_types_provided/2,
         create_path/2, delete_resource/2, delete_completed/2,
         malformed_request/2, options/2, post_is_create/2,
         resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").

-record(context, {userid=undefined}).

%% Webmachine functions

init(Config) ->
    {{trace, "/tmp"}, Config}.  %% debugging code
    %%{ok, Config}.             %% regular code

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET', 'DELETE', 'OPTIONS', 'POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"text/javascript", to_json}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/javascript", to_json}], ReqData, Context}.

create_path(ReqData, Context) ->
    %% {jsonp:wrap(wrq:get_qs_value("callback", ReqData), [{"userid", list_to_binary(new_userid())}]), ReqData, State}.
    Id = new_userid(),
    %% wrq:set_resp_header("Location", string:concat("/userid/", Id), ReqData),
    {string:concat("/userid/", Id), ReqData, Context}.


delete_resource(ReqData, Context) ->
    {delete_userid(wrq:path_info(id, ReqData)), ReqData, Context}.

delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

malformed_request(ReqData, Context) ->
    MF = case wrq:method(ReqData) of
             'GET' ->
                 %% case wrq:get_qs_value("callback", ReqData) of
                 %%     'undefined' -> true;
                 %%     _ -> false
                 %% end;
                 false;
             'POST' ->
                 false;
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

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

resource_exists(ReqData, Context) ->
    {case wrq:method(ReqData) of
        'POST' ->
            false;
        _ ->
            false
     end, ReqData, Context}.

%% Resource-specific functions

delete_userid(Id) ->
    case platformer_db:delete({user, Id}) of
        {atomic, _Result} ->
            true;
        _ ->
            false
    end.

new_userid() ->
    Id = string:concat("platformer_user_", uuid:to_string(uuid:v4())),
    platformer_db:write(#user{id=Id, created=now()}),
    Id.

userid_exists(Id) ->
    length(platformer_db:find(qlc:q([X || X <- mnesia:table(user),
                                           X#user.id == Id]))) > 0.

to_json(ReqData, State) ->
    case wrq:method(ReqData) of
        'GET' ->
            {userid_exists(wrq:path_info(id, ReqData)), ReqData, State};
        'POST' ->
            %% {true, ReqData, State};
            {true, wrq:set_resp_header("Location", wrq:disp_path(ReqData), ReqData), State};
        _ ->
            {true, ReqData, State}
    end.
