%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.
%%
%% This module contains functions used by different Webmachine-based
%% Platformer resources.
-module(platformer_resource_common).

-export([new_propagation_envelope/0, valid_propagation_envelope/1, valid_propagation_envelope/3, support_preflight/2, postprocess_rd/1]).

-include_lib("webmachine/include/webmachine.hrl").

-include_lib("platformer.hrl").

new_propagation_envelope() ->
    #envelope{token=platformer_util:uuid(),
              priority=platformer_util:get_param(memo_priority_max),
              source=platformer_node:my_address()}.

%% @doc Validates a webmachine <code>#wm_reqdata()</code> term for a Platformer query:
%%  Ensure that either:
%%  <ul>
%%    <li>There is a valid X-Platformer-Memo-Token request header, AND</li>
%%    <li>There is a valid X-Platformer-Memo-Priority request header</li>
%%  </ul>
%% OR: Neither of these headers is present.
%% Returns true|false if valid and wm_reqdata() object -- if <code>Explain</code> is
%% true, the wm_reqdata() will carry messages about missing/invalid headers, otherwise it
%% is the original object passed in.
%%
%% @spec valid_propagation_envelope(wm_reqdata(), bool(), bool()) -> {bool(), {string(), integer(), string()}, wm_reqdata()} | {bool(), wm_reqdata()}
valid_propagation_envelope(ReqData, Explain, AllowNone) ->
    {Valid, Invalid, NRD1, Values} =
        verify_headers([{"X-Platformer-Memo-Token", fun(S) -> platformer_memo:is_valid_token(S, "X-Platformer-Memo-Token") end},
                        {"X-Platformer-Memo-Priority", fun(S) -> platformer_memo:is_valid_priority(S, "X-Platformer-Memo-Priority") end}
                       ], ReqData, Explain),
    if
        Valid == 0 andalso Invalid == 0 ->
            if AllowNone     -> true;
               not AllowNone -> {false, NRD1}
            end;
        Invalid > 0 -> {false, NRD1};
        Valid < 2   -> {false, NRD1};
        Valid == 2  -> {true, #envelope{token=proplists:get_value("X-Platformer-Memo-Token", Values),
                                        priority=proplists:get_value("X-Platformer-Memo-Priority", Values),
                                        source=case wrq:get_req_header("X-Platformer-Memo-Source", ReqData) of
                                                   undefined -> "client";
                                                   Source -> Source
                                               end},
                        NRD1}
    end.

%% @doc Same as {@link valid_propagation_envelope/2}, just assumes
%% <code>true</code> for <code>Explain</code> and <code>AllowNone</code>.
valid_propagation_envelope(ReqData) ->
    valid_propagation_envelope(ReqData, true, true).

%% @doc Validate the headers given in <code>HeadersSpec</code>.  If
%% <code>Explain</code> is true, append messages to the response body
%% (via <code>ReqData</code>) to explain problems as necessary.
%%
%% @spec verify_headers(headers_spec(), wm_reqdata(), bool()) -> {all, wm_reqdata(), proplist()} | {bad, wm_reqdata()} | none
verify_headers(HeadersSpec, ReqData, Explain) ->
    verify_headers(HeadersSpec, 0, 0, ReqData, Explain, []).

verify_headers([{HeaderName, Validator}|Rest], Valid, Invalid, ReqData, Explain, ValuesAcc)->
    case wrq:get_req_header(HeaderName, ReqData) of
        undefined ->
            verify_headers(Rest, Valid, Invalid,
                           case Explain of
                               true -> wrq:append_to_response_body("Missing request header " ++ HeaderName ++ "\n", ReqData);
                               false -> ReqData
                           end,
                           Explain,
                           ValuesAcc);
        HeaderValue ->
            case apply(Validator, [HeaderValue]) of
                {false, Reason} ->
                    verify_headers(Rest, Valid, Invalid + 1,
                                   case Explain of
                                       true -> wrq:append_to_response_body(Reason ++ "\n", ReqData);
                                       false -> ReqData
                                   end,
                                   Explain,
                                   ValuesAcc);
                {true, UpdatedValue} ->
                    verify_headers(Rest, Valid + 1, Invalid, ReqData, Explain, [{HeaderName, UpdatedValue}|ValuesAcc])
            end
    end;
verify_headers([], Valid, Invalid, ReqData, _, ValuesAcc) ->
    {Valid, Invalid, ReqData, ValuesAcc}.

%% @doc Compares headers in a <code>#wm_reqdata()</code> term with the
%% allowed <code>AllowedHeaders</code>, and inserts a helpful message
%% in the response body.  This is specifically for use in an
%% <code>options()</code> implementation by a webmachine resource.
%%
%% @spec support_preflight(list(), wm_reqdata()) -> wm_reqdata()
support_preflight(AllowedHeaders, ReqData) ->
    case wrq:get_req_header("Access-Control-Request-Headers", ReqData) of
        undefined ->
            wrq:append_to_response_body("Missing Access-Control-Request-Headers header; cannot support preflight.\n", ReqData);
        ReqHeaders ->
            support_preflight(["Accept"|AllowedHeaders], string:tokens(ReqHeaders, ", "), ReqData)
    end.

support_preflight(AllowedHeaders, [Header|Rest], ReqData) ->
    support_preflight(AllowedHeaders,
                      Rest,
                      case lists:member(Header, AllowedHeaders) of
                          false -> wrq:append_to_response_body("Unknown header " ++ Header ++ "; preflight may fail.\n", ReqData);
                          true -> ReqData
                      end);
support_preflight(_, [], ReqData) -> ReqData.
                              
%% @doc Process a webmachine #wm_reqdata() term in some standard ways:
%%  <ul>
%%    <li>Check for an <code>X-Platformer-Memo-Source</code> request header and add the specified server</li>
%%    <li>Add the response header <code>Access-Control-Allow-Origin: *</code></li>
%%  </ul>
%%
%% @spec postprocess_rd(wm_reqdata()) -> wm_reqdata()
postprocess_rd(ReqData) ->
    case wrq:get_req_header("X-Platformer-Memo-Source", ReqData) of
        undefined -> true;
        Address ->
            case platformer_node:get(platformer_util:md5(Address)) of
                not_found -> platformer_node:create(Address);
                _Server -> true
            end
    end,
    wrq:set_resp_header("Access-Control-Allow-Origin", "*", ReqData).
