%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc User resource.
%%
%% A memo resource can represent a fact, or simply the assertion of the
%% existence of something.  It is an "abstract class" whose concrete
%% instantiations are user_resource, position_resource, etc.

-module(platformer_memo_resource).
-export([init/1, to_json/2]).
-export([allow_missing_post/2, allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2, create_path/2,
         delete_resource/2, delete_completed/2,
         forbidden/2, is_conflict/2,
         malformed_request/2, moved_permanently/2,
         moved_temporarily/2, options/2,
         post_is_create/2, previously_existed/2,
         resource_exists/2, service_available/2]).

-include_lib("webmachine/include/webmachine.hrl").

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
    {case Context#context.id of
         undefined -> ['OPTIONS', 'POST'];
         _ -> ['DELETE', 'HEAD', 'OPTIONS', 'PUT']
     end,
     ReqData, Context}.

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
    {Id, Path} = apply(Context#context.module, create, [Context#context.envelope]),
    {Path, wrq:set_resp_header("Location", Path, ReqData), Context#context{id=Id, path=Path}}.

%% @doc See {@wmdocs}
delete_completed(ReqData, Context) ->
    log4erl:debug("Deleted ~s ~s.", [Context#context.type, Context#context.id]),
    {true, ReqData, Context}.

%% @doc See {@wmdocs}
delete_resource(ReqData, #context{envelope=Envelope} = Context) ->
    log4erl:debug("Deleting ~s ~s, as per propagation request with priority ~B.",
                  [Context#context.type, Context#context.id, Envelope#envelope.priority]),
    {apply(Context#context.module, delete, [Context#context.id, Envelope]) =:= ok,
     ReqData,
     Context}.

%% @doc Handle creation or update of items from PUT requests.
%%
%% See {@wmdocs}
is_conflict(ReqData, Context) ->
    case wrq:method(ReqData) of
        % If the item already exists, we update it; otherwise, we
        % create it and set a Location header so a 201 response will
        % be returned.
        'PUT' ->
            case Context#context.status of
                'active' ->
                    log4erl:debug("PUT to existing ~s; no updating implemented at present.", [Context#context.type]),
                    {false, ReqData, Context};
                'unknown' ->
                    Priority = (Context#context.envelope)#envelope.priority,
                    CreateArgs =
                        if
                            Priority == 0 ->
                                log4erl:debug("PUT of ~s has priority 0; no further propagation.", [Context#context.type]),
                                [Context#context.id];
                            Priority > 0 ->
                                log4erl:debug("PUT of ~s has priority ~B; will be propagated.", [Context#context.type, Priority]),
                                [Context#context.id, Context#context.envelope]
                        end,
                    {_, Path} = apply(Context#context.module, create, CreateArgs),
                    {false, wrq:set_resp_header("Location", Path, ReqData), Context}
            end;
        _ -> {false, ReqData, Context}
    end.

%% @doc Check the memo-token, and record it.  Do not respond to memos
%% with an already-seen token. Also, check syntax of ids for PUT
%% requests to ensure validity.
%%
%% See {@wmdocs}
forbidden(ReqData, Context) ->
    case Context#context.envelope of
        invalid -> {false, ReqData, Context};
        #envelope{token=Token} = Envelope ->
            NC1 = Context#context{envelope = Envelope},
            case platformer_memo:check_token(Token) of
                ok ->
                    log4erl:debug("Received token is ok."),
                    case wrq:method(ReqData) of
                        'PUT' ->
                            case apply(NC1#context.module, is_valid_id, [NC1#context.id]) of
                                true ->
                                    {false, ReqData, NC1};
                                false ->
                                    {true, wrq:append_to_response_body(NC1#context.id ++ " is not a valid id for this type of resource.", ReqData), NC1}
                            end;
                        _ -> {false, ReqData, NC1}
                    end;
                already_seen ->
                    log4erl:debug("Token was already seen: ~s", [Token]),
                    {true, wrq:append_to_response_body("This token has already been seen.", ReqData), NC1}
            end;
        undefined -> {false, ReqData, Context#context{envelope=invalid}}
    end.

%% @doc A PUT request <em>must</em> have a request body, while all
%% others must <em>not</em>.  Also, requests whose envelopes are
%% invalid (see {@link forbidden}) are malformed.
%%
%% See {@wmdocs}
malformed_request(ReqData, Context) ->
    Method = wrq:method(ReqData),
    case Method of
        'OPTIONS' -> {false, ReqData, Context};
        _ ->
            {Envelope, NRD2} =
                case platformer_resource_common:valid_propagation_envelope(ReqData, false, true) of
                    {true, Env, NRD1} ->
                        log4erl:debug("Valid propagation envelope provided."),
                        {Env, NRD1};
                    true ->
                        log4erl:debug("No propagation envelope provided; creating a new one."),
                        {platformer_resource_common:new_propagation_envelope(), ReqData};
                    {false, NRD1} ->
                        log4erl:debug("Invalid propagation envelope provided."),
                        {invalid, NRD1}
                end,
            case Envelope of
                invalid -> {true, NRD2, Context};
                Envelope ->
                    {MF, NRD3, NC1} =
                        case Method of
                            'PUT' ->
                                case wrq:req_body(NRD2) of
                                    <<>> -> {true, wrq:append_to_response_body("PUT must include a request body.", NRD2), Context};
                                    _ -> {false, NRD2, Context}
                                end;
                            _ ->
                                case wrq:req_body(NRD2) of
                                    <<>> -> {false, NRD2, Context};
                                    _ -> {true, wrq:append_to_response_body("Do not include a request body.", NRD2), Context}
                                end
                        end,
                        {MF, platformer_resource_common:postprocess_rd(NRD3), NC1#context{envelope=Envelope}}
            end
    end.

%% @doc See {@wmdocs}
moved_permanently(ReqData, Context) ->
    {false, ReqData, Context}.

%% @doc See {@wmdocs}
moved_temporarily(ReqData, Context) ->
    {false, ReqData, Context}.

%% @doc In addition to just supplying allowed methods and headers, we
%%  also check the headers that are being sent, since browsers doing a
%%  "pre-flight" for some methods will first send along an OPTIONS
%%  request to see if the allowed headers match the ones that the user
%%  wants to send.  We'd like them to get a useful message if there's
%%  a problem, instead of just silently failing.
%%
%% See {@wmdocs}
options(ReqData, Context) ->
    AllowedHeaders = 
        case Context#context.id of
            undefined -> ["X-Platformer-Memo-Token", "X-Platformer-Memo-Priority"];
            _ -> ["X-Platformer-Memo-Token", "X-Platformer-Memo-Priority", "X-Platformer-Memo-Source"]
        end,
    {AllowedMethods, _, _} = allowed_methods(ReqData, Context),
    {[{"Access-Control-Allow-Origin", "*"},
      {"Access-Control-Allow-Methods", string:join(lists:map(fun(A) -> atom_to_list(A) end, AllowedMethods), ", ")},
      {"Access-Control-Allow-Headers", string:join(AllowedHeaders, ", ")}],
     platformer_resource_common:support_preflight(AllowedHeaders, ReqData),
     Context}.

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

%% @doc Here is where we look for a propagation envelope, and if it
%% exists, capture it in the Context.  If it does not exist, we create
%% a new one.
%%
%% See {@wmdocs}
resource_exists(ReqData, Context) ->
    case wrq:method(ReqData) of
        'HEAD' ->
            {Found, Status} = apply(Context#context.module, exists,
                                    [list_to_binary(Context#context.id), Context#context.envelope]),
            {Found, ReqData, Context#context{status=Status}};
        'DELETE' ->
            {Found, Status} = apply(Context#context.module, exists,
                                    [list_to_binary(Context#context.id), platformer_resource_common:new_propagation_envelope()]),
            {Found, ReqData, Context#context{status=Status}};
        'PUT' ->
            {Found, Status} = apply(Context#context.module, exists,
                                    [list_to_binary(Context#context.id)]),
            {Found, ReqData, Context#context{status=Status}};
        _ ->
            {false, ReqData, Context}
    end.

%% @doc We use this function, our earliest access into webmachine's HTTP processing pipeline,
%%  as our way to extract the type of memo being handled.  The first component of the path
%%  names the type.  The type, and the computed name of its corresponding module, are inserted
%%  into the Context object.  If an id has been specified, it is also captured in the Context.
%%  This function always returns true.
%%
%%  See {@wmdocs}
service_available(ReqData, Context) ->
    log4erl:debug("~p request to ~s.", [wrq:method(ReqData), wrq:raw_path(ReqData)]),
    Type = hd(string:tokens(wrq:path(ReqData), "/")),
    Module = list_to_atom(string:concat("platformer_", Type)),
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
             wrq:set_resp_body(apply(Context#context.module, to_json, [Context#context.id]), ReqData),
             Context};
        _ ->
            {<<>>, ReqData, Context}
    end.

