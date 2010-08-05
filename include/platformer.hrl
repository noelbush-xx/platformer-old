%% A user of Platformer.
-record(platformer_user, {id :: string(),              % a unique id (v4 uuid)
                          status :: active | deleted,  % active | deleted
                          last_modified :: integer(),  % when last modified by node retaining this record
                          source :: string()           % url of node who told us about this user
                         }).

%% A Platformer node.
-record(platformer_node, {id,             % an id for the node (formed using a prefix and an md5 hash of the address)
                          scheme,         % http | https
                          host,           % hostname (e.g., example.com)
                          port,           % port (e.g., 8000)
                          status,         % active | deleted
                          rating,         % 0 (worst) - 100 (best)
                          last_modified   % when last modified by node retaining this record
                         }).

%% A token sent along to track memos.
-record(platformer_token, {id,            % a unique id (v4 uuid)
                           received       % when the memo bearing this token was received
                          }).

%% A subset of what's in a full node record, for announcing self.
-record(nodespec, {scheme,       % http | https
                   host,         % hostname (e.g., example.com)
                   port          % port (e.g., 8000)
                  }).


%% A set of information that follows around a memo item when it is propagated.
-record(envelope, {token :: string(),         % a (reasonably) unique string to identify a message
                   priority :: integer(),     % an integer indicating how much farther a message should propagate
                   source :: string()         % the origin of the message
                  }).

