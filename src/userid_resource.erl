%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.

-module(userid_resource).
-export([init/1, to_json/2]).
-export([allow_missing_post/2, allowed_methods/2,
         content_types_provided/2,
         delete_resource/2, delete_completed/2,
         malformed_request/2, options/2
         %%resource_exists/2 <-- TODO: maybe implement this
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").

-record(context, {config, callback, userid}).

%% Webmachine functions

init(Config) ->
    {{trace, "/tmp"}, #context{config=Config}}.  %% debugging code
    %%{ok, #context{config=Config}}.             %% regular code

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET', 'DELETE', 'OPTIONS'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

delete_resource(ReqData, Context) ->
    {delete_userid(wrq:path_info(id, ReqData)), ReqData, Context}.

delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

malformed_request(ReqData, Context) ->
    {Malformed, Message, NewContext} =
        case wrq:method(ReqData) of
            'GET' ->
                case wrq:get_qs_value("callback", ReqData) of
                    undefined ->
                        {true, "No callback specified.", Context};
                    Callback ->
                        {false, undefined, Context#context{callback=Callback}}
                end;
            'DELETE' ->
                case wrq:path_info(id, ReqData) of
                    undefined -> 
                        {true, "No userid specified.", Context};
                    _ ->
                        {false, undefined, Context}
                end;
            'OPTIONS' ->
                {false, undefined, Context}
        end,
    NewReqData = 
        if
            Message =/= undefined -> wrq:append_to_response_body(Message, ReqData);
            true -> ReqData
        end,
    {Malformed, NewReqData, NewContext}.

options(ReqData, Context) ->
    {[{"Access-Control-Allow-Origin", "*"},
      {"Access-Control-Allow-Methods", "GET, DELETE, OPTIONS"}
     ], ReqData, Context}.

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

to_json(ReqData, Context) ->
    case wrq:method(ReqData) of
        'GET' ->
            case wrq:path_info('id', ReqData) of
                undefined ->
                    Id = new_userid(),
                    {true,
                     wrq:set_resp_header("Location", string:concat("/userid/", Id),
                                         wrq:set_resp_body(
                                           json:wrap(Context#context.callback, [{userid, Id}]),
                                           ReqData)),
                     Context};
                RequestedId ->
                    {userid_exists(RequestedId), ReqData, Context}
            end;
        _ ->
            {"", ReqData, Context}
    end.
