%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.
%%
%% This module contains functions used by different platformer resources.
-module(platformer.pfr).

-export([valid_extended_query_request/1, postprocess_rd/1]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("platformer.hrl").

%% Validate a webmachine #wm_reqdata() term for a Platformer query:
%% - Ensure there is an X-Platformer-Query-Token request header
%% - Ensure there is an X-Platformer-Query-Age request header
%% Returns true|false if valid and modified RD.
valid_extended_query_request(ReqData) ->
    verify_headers(["X-Platformer-Query-Token", "X-Platformer-Query-Age"], ReqData).

verify_headers(Headers, ReqData) ->
    verify_headers(Headers, true, ReqData).

verify_headers([Header|Rest], Valid, ReqData)->
    verify_headers(Rest, Valid and (wrq:get_req_header(Header, ReqData) =/= undefined), ReqData);
verify_headers([], Valid, _ReqData) ->
    Valid.

%% Process a webmachine #wm_reqdata() term in some standard ways:
%% - Check for an X-Platformer-Node request header and add the specified server
%% - Add the response header Access-Control-Allow-Origin: *
postprocess_rd(ReqData) ->
    case wrq:get_req_header("X-Platformer-Node", ReqData) of
        undefined -> true;
        Address ->
            case pfnode:get(util:md5(Address)) of
                not_found -> pfnode:create(Address);
                _Server -> true
            end
    end,
    wrq:set_resp_header("Access-Control-Allow-Origin", "*", ReqData).
