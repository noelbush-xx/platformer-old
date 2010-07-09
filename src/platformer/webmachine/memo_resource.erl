%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc User resource.
%%
%% A memo resource can represent a fact, or simply the assertion of the
%% existence of something.  It is an "abstract class" whose concrete
%% instantiations are user_resource, position_resource, etc.

-module(platformer.webmachine.memo_resource).
-export([init/1, to_json/2]).
-export([allow_missing_post/2, allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2, create_path/2,
         delete_resource/2, delete_completed/2,
         malformed_request/2, moved_permanently/2,
         moved_temporarily/2, options/2,
         post_is_create/2, previously_existed/2,
         resource_exists/2, service_available/2]).

-import(dict).
-import(lists).
-import(string).

-import(jsonerl).
-import(log4erl).
-import(wrq).

-import(platformer.core.util).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("jsonerl.hrl").

-include_lib("platformer.hrl").

%% @doc The <code>context</code> record for a memo_resource is used to hold on to
%%  information about the resource as requests about it are processed along the
%%  HTTP pipeline.  It contains the following:
%%
%% <dl>
%%   <dt>config</dt>    <dd>The configuration passed from webmachine</dd>
%%   <dt>type</dt>      <dd>The type of the memo (user, position, etc.)</dd>
%%   <dt>module</dt>    <dd>The module that contains specific handling for the memo type</dd>
%%   <dt>id</dt>        <dd>The id for the memo as generated (in a POST) or specified by a HEAD, GET, DELETE or PUT
%%   <dt>status</dt>    <dd>An atom describing the disposition of the memo (active, deleted, ...)</dd>
%%   <dt>body</dt>      <dd>The request body sent, if any</dd>
%%   <dt>record</dt>    <dd>The record retrieved or created that pertains to the resource</dd>
%%   <dt>envelope</dt>  <dd>The message token, priority and status carried along by the memo as it propagates</dd>
%% </dl>
-record(context, {config, type, module, id, path, status, body, record, envelope}).

%% @doc See {@wmdocs}
init(Config) ->
    {{trace, "/tmp/platformer/" ++ atom_to_list(node())}, #context{config=Config}}.  %% debugging code
    %%{ok, #context{config=Config}}.                                                 %% regular code

%% @doc See {@wmdocs}
allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc See {@wmdocs}
allowed_methods(ReqData, Context) ->
    {['DELETE', 'HEAD', 'OPTIONS', 'POST', 'PUT'], ReqData, Context}.

%% @doc See {@wmdocs}
content_types_accepted(ReqData, Context) ->
    {[{"application/x-www-form-urlencoded", to_json},  %% default as sent by jquery & forms in general
      {"application/octet-stream", to_json},           %% default set by webmachine when Content-Type isn't supplied
      {"text/plain", to_json}                          %% used by PUT
     ], ReqData, Context}.

%% @doc See {@wmdocs}
content_types_provided(ReqData, Context) ->
    {[{"text/javascript", to_json}], ReqData, Context}.

%% @doc See {@wmdocs}
create_path(ReqData, Context) ->
    {Id, Path} = apply(Context#context.module, create, []),
    {Path, wrq:set_resp_header("Location", Path, ReqData), Context#context{id=Id, path=Path}}.

%% @doc See {@wmdocs}
delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc See {@wmdocs}
delete_resource(ReqData, Context) ->
    {apply(Context#context.module, delete, [list_to_binary(Context#context.id)]), ReqData, Context}.

%% @doc See {@wmdocs}
malformed_request(ReqData, Context) ->
    {MF, NewReqData, NewContext} =
        case wrq:method(ReqData) of
            % A HEAD or DELETE request is malformed if it is missing an id.
            M when M =:= 'HEAD' orelse M =:= 'DELETE' ->
                case Context#context.id of
                    undefined -> 
                        {true,
                         wrq:append_to_response_body("No id specified.", ReqData),
                         Context};
                    _ ->
                        {false, ReqData, Context}
                end;
            % An OPTIONS request can never be malformed.
            'OPTIONS' ->
                {false, ReqData, Context};
            % A POST is malformed if it contains a request body or does not specify an id.
            'POST' ->
                {MF1, NRD1, NC1} =
                    case wrq:req_body(ReqData) of
                        <<>> -> {false, ReqData, Context};
                        _ -> {true, wrq:append_to_response_body("Do not include a request body.", ReqData), Context}
                    end,
                %% Be sure the path is right.
                case Context#context.id of
                    undefined -> {false and MF1, NRD1, NC1};
                    _ -> {true, wrq:append_to_response_body("Do not POST to an existing resource.", NRD1), NC1}
                end;
            % A PUT is malformed if it is missing an id and or does not have valid message propagation headers.
            'PUT' ->
                case Context#context.id of
                    undefined -> {true, wrq:append_to_response_body("Missing id.", ReqData), Context};
                    Id ->
                        {ValidHeaders, NRD1} = common:valid_propagation_envelope(ReqData),
                        case apply(Context#context.module, is_valid_id, [Id]) of
                            false -> {true, wrq:append_to_response_body(Id ++ " is not a valid id.", NRD1), Context};
                            true -> {not ValidHeaders, NRD1, Context}
                        end
                end
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
                {"Access-Control-Allow-Methods", "OPTIONS, POST, PUT"};
            _ ->
                [{"Access-Control-Allow-Methods", "HEAD, DELETE, OPTIONS"},
                 {"Access-Control-Allow-Headers",
                  "X-Platformer-Message-Token, X-Platformer-Message-Priority, X-Platformer-Message-Source"}]
        end]),
     ReqData, Context}.

%% @doc See {@wmdocs}
post_is_create(ReqData, Context) ->
    {case Context#context.id of
         undefined -> true;
         Id ->
             case apply(Context#context.module, exists, [{Id, false}]) of
                 {false, unknown} ->
                     log4erl:debug("Received POST with new ~s: ~s", [Context#context.type, Id]),
                     true;
                 {true, active} ->
                     log4erl:debug("Received POST with previously known ~s: ~s", [Context#context.type, Id]),
                     false;
                 {false, deleted} ->
                     log4erl:debug("Received POST with deleted ~s: ~s", [Context#context.type, Id]),
                     false
             end
     end,
     ReqData, Context}.

%% @doc See {@wmdocs}
previously_existed(ReqData, Context) ->
    {case Context#context.status of deleted -> true; _ -> false end,
     ReqData,
     Context}.

%% @doc See {@wmdocs}
resource_exists(ReqData, Context) ->
    {Exists, NewContext} =
        case wrq:method(ReqData) of
            M when M =:= 'DELETE' orelse M =:= 'HEAD' ->
                %% Only check other servers if there is a valid propagation envelope.
                {Valid, _} = common:valid_propagation_envelope(ReqData),
                {Found, Status} = 
                    apply(Context#context.module, exists, [{list_to_binary(Context#context.id), Valid}]),
                {Found, Context#context{status=Status}};
            _ ->
                {false, Context}
        end,
    {Exists, ReqData, NewContext}.

%% @doc We use this function, our earliest access into webmachine's HTTP processing pipeline,
%%  as our way to extract the type of memo being handled.  The first component of the path
%%  names the type.  The type, and the computed name of its corresponding module, are inserted
%%  into the Context object.  If an id has been specified, it is also captured in the Context.
%%  This function always returns true.
%%
%%  See {@wmdocs}
service_available(ReqData, Context) ->
    Type = hd(string:tokens(wrq:path(ReqData), "/")),
    Module = list_to_atom(string:concat("platformer.core.", Type)),
    NC1 = Context#context{type=Type, module=Module},
    {true, ReqData, case wrq:path_info('id', ReqData) of
                        undefined -> NC1;
                        Id -> NC1#context{id=Id}
                    end}.

%% @spec to_json(wm_reqdata(), term()) -> {string(), rd(), term()}
to_json(ReqData, Context) ->
    case wrq:method(ReqData) of
        'POST' ->
            {true,
             wrq:set_resp_body(jsonerl:encode({{Context#context.type, {{id, Context#context.id}}}}), ReqData),
             Context};
        % This seems undesirable, but maybe it's how we have to match the webmachine flow?
        % For a PUT, we create the item right here, and add a Location header so it will
        % send back a 201 response.
        'PUT' ->
            % The presence of these headers has already been verified in malformed_request().
            Token = wrq:get_req_header("X-Platformer-Message-Token", ReqData),
            Priority = lists:max([list_to_integer(wrq:get_req_header("X-Platformer-Priority", ReqData)),
                                  util:get_param(propagation_priority, 3)]),
            Source = wrq:get_req_header("X-Platformer-Message-Source", ReqData),
            if
                Priority == 0 ->
                    log4erl:debug("Received PUT id with priority 0; nothing to create."),
                    {true, ReqData, Context};
                % otherwise it must be less than or equal to the max. priority (see above)
                true ->
                    log4erl:debug("Received PUT id with priority ~B; creating a new one.", [Priority]),
                    {_, Path} = apply(Context#context.module, create, [Context#context.id, {Token, Priority - 1, Source}]),
                    {true,
                     wrq:set_resp_header("Location", Path, ReqData),
                     Context}
            end;
        _ ->
            {<<>>, ReqData, Context}
    end.

