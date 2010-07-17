-module(platformer.webmachine.error_handler).

-export([render_error/3]).

-import(eq).
-import(error_logger).
-import(io_lib).

-import(log4erl).

render_error(Code, Req, Reason) ->
    %%log4erl:debug("ERROR.~nCode: ~p~nReq: ~p~nReason: ~p~n", [Code, Req, Reason]),
    case Req:has_response_body() of
        {true,_} -> Req:response_body();
        {false,_} -> render_error_body(Code, Req:trim_state(), Reason)
    end.

render_error_body(404, Req, _Reason) ->
    case Req:method() of
        {'HEAD', _} ->
            {_, ReqState} = Req:get_reqdata(),
            {<<>>, ReqState};
        _ ->
            {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
            {<<"<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1>The requested item was not found on this server.</body></html>">>, ReqState}
    end;

render_error_body(500, Req, Reason) ->
    case Req:method() of
        {'HEAD', _} ->
            {_, ReqState} = Req:get_reqdata(),
            {<<>>, ReqState};
        _ ->
            {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
            {Path,_} = Req:path(),
            error_logger:error_msg("webmachine error: path=~p~n~p~n", [Path, Reason]),
            STString = io_lib:format("~p", [Reason]),
            ErrorStart = "<html><head><title>500 Internal Server Error</title></head><body><h1>Internal Server Error</h1>The server encountered an error while processing this request:<br><pre>",
            ErrorEnd = "</pre></body></html>",
            ErrorIOList = [ErrorStart,STString,ErrorEnd],
            {erlang:iolist_to_binary(ErrorIOList), ReqState}
    end;

render_error_body(501, Req, _Reason) ->
    case Req:method() of
        {'HEAD', _} ->
            {_, ReqState} = Req:get_reqdata(),
            {<<>>, ReqState};
        _ ->
            {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
            {Method,_} = Req:method(),
            error_logger:error_msg("Webmachine does not support method ~p~n",
                                   [Method]),
            ErrorStr = io_lib:format("<html><head><title>501 Not Implemented</title>"
                                     "</head><body><h1>Internal Server Error</h1>"
                                     "The server does not support the ~p method."
                                     "</body></html>",
                                     [Method]),
            {erlang:iolist_to_binary(ErrorStr), ReqState}
    end;

render_error_body(503, Req, _Reason) ->
    case Req:method() of
        {'HEAD', _} ->
            {_, ReqState} = Req:get_reqdata(),
            {<<>>, ReqState};
        _ ->
            {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
            error_logger:error_msg("Webmachine cannot fulfill the request at this time"),
            ErrorStr = "<html><head><title>503 Service Unavailable</title>"
                "</head><body><h1>Service Unavailable</h1>"
                "The server is currently unable to handle "
                "the request due to a temporary overloading "
                "or maintenance of the server."
                "</body></html>",
            {list_to_binary(ErrorStr), ReqState}
    end.

