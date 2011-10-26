%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%%
%% @doc A user resource has a few differences from a {@link
%% platformer_memo_resource. memo resource}.

-module(platformer_user_resource).
-extends(platformer_memo_resource).

-export([allowed_methods/2, create_path/2,
         post_is_create/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("platformer.hrl").

%% @doc A user can be created with POST, so we allow this method on a
%% URI with no id. See {@wmdocs}
allowed_methods(ReqData, Context) ->
    {case Context#context.id of
         undefined -> ['OPTIONS', 'POST'];
         _ -> ['DELETE', 'HEAD', 'OPTIONS', 'PUT']
     end,
     ReqData, Context}.

%% @doc A user resource can be created with a simple (POST) request
%% with no id -- the id is generated.  Its create function requires no
%% arguments other than the envelope.  This is different from most
%% memo types, so this function is overridden. See {@wmdocs}
create_path(ReqData, Context) ->
    {Id, Path} = apply(Context#context.memo_module, create, [Context#context.envelope]),
    {Path, wrq:set_resp_header("Location", Path, ReqData), Context#context{id=Id, path=Path}}.

%% @doc See {@wmdocs}
post_is_create(ReqData, Context) ->
    Id = Context#context.id,
    platformer_memo_resource:post_is_create(Id =:= undefined orelse Id, ReqData, Context).
