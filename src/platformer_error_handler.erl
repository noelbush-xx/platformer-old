-module(platformer_error_handler).

-export([render_error/3]).

render_error(Code, Req, Reason) ->
    %%log4erl:debug("ERROR.~nCode: ~p~nReq: ~p~nReason: ~s~n", [Code, Req, Reason]),
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
            {ok, ReqState} = Req:add_response_header("Content-Type", "text/plain"),
            {<<"The requested item was not found on this server.">>, ReqState}
    end;

render_error_body(500, Req, Reason) ->
    case Req:method() of
        {'HEAD', _} ->
            {_, ReqState} = Req:get_reqdata(),
            {<<>>, ReqState};
        _ ->
            {ok, ReqState} = Req:add_response_header("Content-Type", "text/plain"),
            {Path,_} = Req:path(),
            error_logger:error_msg(
              case is_list(Reason) of
                  true -> "webmachine error: path=~p~n~s~n";
                  false -> "webmachine error: path=~p~n~p~n" end,
              [Path, Reason]),
            STString = io_lib:format("~p", [Reason]),
            ErrorIOList = ["The server encountered an error while processing this request:\n",STString],
            {erlang:iolist_to_binary(ErrorIOList), ReqState}
    end;

render_error_body(501, Req, _Reason) ->
    case Req:method() of
        {'HEAD', _} ->
            {_, ReqState} = Req:get_reqdata(),
            {<<>>, ReqState};
        _ ->
            {ok, ReqState} = Req:add_response_header("Content-Type", "text/plain"),
            {Method,_} = Req:method(),
            error_logger:error_msg("Webmachine does not support method ~p~n", [Method]),
            ErrorStr = io_lib:format("The server does not support the ~p method.", [Method]),
            {erlang:iolist_to_binary(ErrorStr), ReqState}
    end;

render_error_body(503, Req, _Reason) ->
    case Req:method() of
        {'HEAD', _} ->
            {_, ReqState} = Req:get_reqdata(),
            {<<>>, ReqState};
        _ ->
            {ok, ReqState} = Req:add_response_header("Content-Type", "text/html"),
            error_logger:error_msg("Webmachine cannot fulfill the request at this time."),
            ErrorStr = "The server is currently unable to handle "
                "the request due to a temporary overloading "
                "or maintenance of the server.",
            {list_to_binary(ErrorStr), ReqState}
    end.

