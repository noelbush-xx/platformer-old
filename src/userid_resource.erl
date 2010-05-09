%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Userid resource.

-module(userid_resource).
-export([init/1, to_json/2]).
-export([allowed_methods/2, content_types_provided/2, malformed_request/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
   {{trace, "/tmp"}, Config}.  %% debugging code
   %%{ok, Config}.             %% regular code

allowed_methods(ReqData, Context) ->
  {['GET'], ReqData, Context}.
    
content_types_provided(ReqData, Context) ->
  {[{"text/javascript", to_json}], ReqData, Context}.
  
malformed_request(ReqData, Context) ->
  MF = case wrq:get_qs_value("callback", ReqData) of
    'undefined' -> true;
    _ -> false
  end,
  {MF, ReqData, Context}.

to_json(ReqData, State) ->
  {jsonp:wrap(wrq:get_qs_value("callback", ReqData), [{'userid', uuid:new()}]), ReqData, State}.
