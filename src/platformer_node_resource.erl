%% @author Noel Bush <noel@platformer.org>
%% @copyright 2010 Noel Bush.
%% @doc Node resource.
%%
%% This module handles creating and deleting node records in Platformer.
%%
%% Method      URI                      -->  Successful Response
%% GET         /node/list                  200 text/javascript list of nodes (length of list is up to node)
%% POST        /node (+json-enc address)   201 Location: URI of new node ("/node/id")
%% HEAD        /node/id                    200 (if node exists)
%% DELETE      /node/id                    204
%% OPTIONS     /node                       200 Access-Control-Allow-Methods: POST, OPTIONS
%% OPTIONS     /node/id                    200 Access-Control-Allow-Methods: HEAD, DELETE, OPTIONS

-module(platformer_node_resource).
-extends(platformer_memo_resource).

-export([init/1, to_json/2]).
-export([allowed_methods/2, malformed_request/2,
         options/2, post_is_create/2, previously_existed/2,
         process_post/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("jsonerl.hrl").
-include_lib("platformer.hrl").

%% This is used locally for ease in constructing a list of nodes for a "/nodes/list" query.
-record(nodes, {nodes}).


init(Config) ->
    {{trace, "/tmp/platformer/" ++ atom_to_list(node())}, #context{config=Config}}.  %% debugging code
    %%{ok, #context{config=Config}}.             %% regular code


allowed_methods(ReqData, Context) ->
    case wrq:raw_path(ReqData) of
        "/node/list" -> {['OPTIONS', 'GET'], ReqData, Context};
        "/node" -> {['OPTIONS', 'POST'], ReqData, Context};
        _ -> platformer_memo_resource:allowed_methods(ReqData, Context)
    end.


%% create_path(ReqData, Context) ->
%%     case platformer_node_memo:create(Context#context.record) of
%%         {Ok, Node} when Ok =:= ok orelse Ok =:= already_exists ->
%%             Path = platformer_node_memo:get_path(Node),
%%             {Path, wrq:set_resp_header("Location", Path, ReqData), Context#context{record=Node}};
%%         {error, Error} ->
%%             {"", wrq:append_to_response_body("Could not create new node.\n" ++ Error, ReqData), Context}
%%     end.

malformed_request(ReqData, Context) ->
    {MF, NewReqData, NewContext} =
        case wrq:method(ReqData) of
            'DELETE' ->
                case wrq:path_info(id, ReqData) of
                    undefined ->
                        {true, wrq:append_to_response_body("No node id specified.", ReqData), Context};
                    _ ->
                        {false, ReqData, Context}
                end;
            'GET' ->
                {false, ReqData, Context};
            'HEAD' ->
                {case wrq:path_info(id, ReqData) of undefined -> true; _ -> false end, ReqData, Context};
            'OPTIONS' ->
                {false, ReqData, Context};
            'POST' ->
                %% Check that the body contains a json object with the node spec
                Body = wrq:req_body(ReqData),
                {MF1, NRD1, NC1} =
                    case platformer_node_memo:from_json(Body) of
                        #platformer_node{} = Record ->
                            {false, ReqData, Context#context{record=Record}};
                        error ->
                            {true, wrq:append_to_response_body("Invalid or missing node specification."), Context}
                    end,
                %% Be sure the path is right.
                case wrq:path_info(id, ReqData) of
                    undefined -> {false and MF1, NRD1, NC1};
                    _ -> {true, wrq:append_to_response_body("Do not POST to an existing node.", NRD1), NC1}
                end
        end,
    {MF, platformer_resource_common:postprocess_rd(NewReqData), NewContext}.

options(ReqData, Context) ->
    {[{"Access-Control-Allow-Origin", "*"},
      {"Access-Control-Allow-Methods", string:join(?MODULE:allowed_methods(ReqData, Context))}],
     ReqData, Context}.

%% @doc POST is only create if the request body describes a node we don't yet know.
post_is_create(ReqData, Context) ->
    {platformer_node_memo:get(Context#context.record#platformer_node.id) =:= not_found, ReqData, Context}.

previously_existed(ReqData, Context) ->
    {platformer_node_memo:get(Context#context.record#platformer_node.id) /= not_found, ReqData, Context}.

%% This will be called only if the node record has already been
%% found, in which case we just want to send a 303 "See Other"
%% response.
process_post(ReqData, Context) ->
    {true, wrq:do_redirect(true, wrq:set_resp_header("Location", platformer_node_memo:get_path(Context#context.record), ReqData)), Context}.

resource_exists(ReqData, Context) ->
    case wrq:raw_path(ReqData) of
        "/node/list" ->
            case wrq:method(ReqData) of
                'GET' -> {true, ReqData, Context};
                _ -> {false, ReqData, Context}
            end;
        _ ->
            platformer_memo_resource:resource_exists(ReqData, Context)
    end.

%% @spec to_json(rd(), term()) -> {string(), rd(), term()}
to_json(ReqData, Context) ->
    case wrq:raw_path(ReqData) =:= "/node/list" andalso
        wrq:method(ReqData) =:= 'GET' of
        true ->
            %% Of the known nodes with rating 75 or greater,
            %% share a random sample of 25%.
            NodeRecords = platformer_node_memo:get_random_list({percentage, 25}, [{min_rating, 75}]),
            Nodes = [?record_to_struct(platformer_node, Node) || Node <- NodeRecords],
            {jsonerl:encode(?record_to_struct(nodes, #nodes{nodes=Nodes})), ReqData, Context};
        false ->
            platformer_memo_resource:to_json(ReqData, Context)
    end.

