%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.
%%
%% This module contains functions used by different platformer resources.
-module(platformer.webmachine.common).

-export([valid_propagation_envelope/1, postprocess_rd/1]).

-import(wrq).

-import(platformer.core.node).
-import(platformer.core.util).

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("platformer.hrl").

%% @doc Validates a webmachine <code>#wm_reqdata()</code> term for a Platformer query:
%%  <ul>
%%    <li>Ensure there is an X-Platformer-Message-Token request header</li>
%%    <li>Ensure there is an X-Platformer-Message-Priority request header</li>
%%  </ul>
%% Returns true|false if valid and modified RD.
%%
%% @spec valid_propagation_envelope(#wm_reqdata()) -> {bool(), #wm_reqdata()}
valid_propagation_envelope(ReqData) ->
    verify_headers(["X-Platformer-Message-Token", "X-Platformer-Message-Priority, X-Platformer-Message-Source"], ReqData).

verify_headers(Headers, ReqData) ->
    verify_headers(Headers, true, ReqData).

verify_headers([Header|Rest], Valid, ReqData)->
    case wrq:get_req_header(Header, ReqData) of
        undefined -> verify_headers(Rest, false, wrq:append_to_response_body("Missing request header " ++ Header, ReqData));
        _ -> verify_headers(Rest, Valid, ReqData)
    end;
verify_headers([], Valid, ReqData) ->
    {Valid, ReqData}.

%% @doc Process a webmachine #wm_reqdata() term in some standard ways:
%%  <ul>
%%    <li>Check for an <code>X-Platformer-Message-Source</code> request header and add the specified server</li>
%%    <li>Add the response header <code>Access-Control-Allow-Origin: *</code></li>
%%  </ul>
%%
%% @spec postprocess_rd(wm_reqdata()) -> wm_reqdata()
postprocess_rd(ReqData) ->
    case wrq:get_req_header("X-Platformer-Message-Source", ReqData) of
        undefined -> true;
        Address ->
            case node:get(util:md5(Address)) of
                not_found -> node:create(Address);
                _Server -> true
            end
    end,
    wrq:set_resp_header("Access-Control-Allow-Origin", "*", ReqData).
