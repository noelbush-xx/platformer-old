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
-export([init/1, to_json/2]).
-export([accept_content/2, allow_missing_post/2, allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2, create_path/2,
         delete_resource/2, delete_completed/2,
         malformed_request/2, moved_permanently/2,
         moved_temporarily/2, options/2,
         post_is_create/2, previously_existed/2,
         process_post/2,
         resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("jsonerl.hrl").

-include_lib("platformer.hrl").

-record(context, {config,
                  body,     %% in a POST, this will hold the parsed body
                  record    %% in a GET, this will contain the retrieved node record
                 }).

%% This is used locally for ease in constructing a list of nodes for a "/nodes/list" query.
-record(nodes, {nodes}).

init(Config) ->
    %%{{trace, "/tmp/platformer/" ++ atom_to_list(node())}, #context{config=Config}}.  %% debugging code
    {ok, #context{config=Config}}.             %% regular code

accept_content(ReqData, Context) ->
    {true, ReqData, Context}.

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['DELETE', 'GET', 'HEAD', 'OPTIONS', 'POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"text/javascript", accept_content}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/javascript", to_json}], ReqData, Context}.

create_path(ReqData, Context) ->
    case platformer_node:create(Context#context.record) of
        {Ok, Node} when Ok =:= ok orelse Ok =:= already_exists ->
            Path = platformer_node:get_path(Node),
            {Path, wrq:set_resp_header("Location", Path, ReqData), Context#context{record=Node}};
        {error, Error} ->
            {"", wrq:append_to_response_body("Could not create new node.\n" ++ Error, ReqData), Context}
    end.

delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.

delete_resource(ReqData, Context) ->
    {platformer_node:delete(wrq:path_info(id, ReqData)), ReqData, Context}.

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
                    try ?json_to_record(nodespec, binary_to_list(Body)) of
                        %% Since we have to construct the record to determine its validity, we'll hang onto it
                        %% (We fix the scheme as an atom while we're at it.)
                        #nodespec{scheme=Scheme, host=Host, port=Port} ->
                            Record = #platformer_node{scheme = binary_to_atom(Scheme, latin1), host = Host, port = Port},
                            {false, ReqData,
                             Context#context{record=Record}}
                    catch error ->
                            {true, wrq:append_to_response_body("Invalid or missing node specification."), Context}
                    end,
                %% Be sure the path is right.
                case wrq:path_info(id, ReqData) of
                    undefined -> {false and MF1, NRD1, NC1};
                    _ -> {true, wrq:append_to_response_body("Do not POST to an existing node.", NRD1), NC1}
                end
        end,
    {MF, platformer_resource_common:postprocess_rd(NewReqData), NewContext}.

moved_permanently(ReqData, Context) ->
    {false, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    {false, ReqData, Context}.

options(ReqData, Context) ->
    {[{"Access-Control-Allow-Origin", "*"},
      case wrq:path_info(id, ReqData) of
          undefined ->
              {"Access-Control-Allow-Methods", "OPTIONS, POST"};
          _ ->
              {"Access-Control-Allow-Methods", "HEAD, DELETE, OPTIONS"}
      end],
     ReqData, Context}.

%% POST is only create if the request body describes a node we don't yet know.
post_is_create(ReqData, Context) ->
    Record = Context#context.record,
    {case platformer_node:get(Record) of
         not_found ->
             %% log4erl:debug("Received POST with new platformer_node: ~p", [platformer_node:get_address(Record)]),
             true;
         _Node ->
             %% log4erl:debug("Received POST with previously known platformer_node: ~p", [platformer_node:get_address(Node)]),
             false
     end,
     ReqData, Context}.

previously_existed(ReqData, Context) ->
    Node = Context#context.record,
    {case Node#platformer_node.status of deleted -> true; _ -> false end,
     ReqData,
     Context}.

%% This will be called only if the node record has already been
%% found, in which case we just want to send a 303 "See Other"
%% response.
process_post(ReqData, Context) ->
    {true, wrq:do_redirect(true, wrq:set_resp_header("Location", platformer_node:get_path(Context#context.record), ReqData)), Context}.

resource_exists(ReqData, Context) ->
    resource_exists(wrq:method(ReqData), ReqData, Context).

resource_exists(Method, ReqData, Context) when Method =:= 'GET'; Method =:= 'HEAD'; Method =:= 'DELETE' ->
    case wrq:raw_path(ReqData) of
        "/node/list" -> {true, ReqData, Context};
        _ ->
            case platformer_node:get(wrq:path_info(id, ReqData)) of
                not_found -> {false, ReqData, Context};
                Node -> {true, ReqData, Context#context{record = Node}}
            end
    end;
resource_exists('POST', ReqData, Context) -> {false, ReqData, Context};
resource_exists(_, ReqData, Context) -> {true, ReqData, Context}.


%% @spec to_json(rd(), term()) -> {string(), rd(), term()}
to_json(ReqData, Context) ->
    {case wrq:method(ReqData) of
         'GET' ->
             case wrq:path_info(id, ReqData) of
                 undefined ->
                     %% Of the known nodes with rating 75 or greater,
                     %% share a random sample of 25%.
                     NodeRecords = platformer_node:get_random_list({percentage, 25}, [{min_rating, 75}]),
                     Nodes = [?record_to_struct(platformer_node, Node) || Node <- NodeRecords],
                     jsonerl:encode(?record_to_struct(nodes, #nodes{nodes=Nodes}));
                 Id ->
                     case platformer_node:get(Id) of
                         undefined -> <<>>;
                         Node -> ?record_to_json(platformer_node, Node)
                     end
             end;
         _ -> <<>>
     end,
     ReqData, Context}.

