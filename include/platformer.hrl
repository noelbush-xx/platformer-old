-define(TOKEN_HEADER, "X-Platformer-Memo-Token").
-define(PRIORITY_HEADER, "X-Platformer-Memo-Priority").
-define(SOURCE_HEADER, "X-Platformer-Memo-Source").

%% @doc A user of Platformer.
-record(platformer_user, {id :: binary(),                  % a unique id (v4 uuid)
                          status :: 'active' | 'deleted',
                          last_modified :: integer(),      % when last modified by node retaining this record
                          source                           % url of node who told us about this user
                         }).

%% @doc A Platformer node.
-record(platformer_node, {id :: binary(),                 % (formed using a prefix and an md5 hash of the address)
                          scheme :: 'http' | 'https',
                          host :: binary(),               % hostname (e.g., example.com)
                          port :: integer(),              % port (e.g., 8000)
                          status :: 'active' | 'deleted',
                          rating :: integer(),            % 0 (worst) - 100 (best)
                          last_modified :: integer()      % when last modified by node retaining this record
                         }).

%% @doc A tag connects a userid with an element.  The userid and elementid are encrypted.
-record(platformer_tag, {id :: binary(),
                         userid :: binary(),
                         elementid :: binary(),
                         created :: integer()
                        }).

-record(platformer_element, {id :: binary(),
                             text :: binary(),
                             created :: integer()
                            }).

-record(platformer_claim, {id :: binary(),
                           text :: binary(),
                           created :: integer()
                          }).

-record(platformer_interpretation, {id :: binary(),
                                    text :: binary(),
                                    created :: integer()
                                   }).

%% @doc A token sent along to track memos.
-record(platformer_token, {id,            % a unique id (v4 uuid)
                           received       % when the memo bearing this token was received
                          }).


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

%% @doc A set of information that follows around a memo item when it is propagated.
-record(envelope, {token :: string(),         % a (reasonably) unique string to identify a message
                   priority :: integer(),     % an integer indicating how much farther a message should propagate
                   source :: string()         % the origin of the message
                  }).

