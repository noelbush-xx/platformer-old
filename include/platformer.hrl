%% A user of Platformer.
-record(pfuser, {id,             % a unique id (v4 uuid)
                 status,         % active | deleted
                 last_modified,  % when last modified by node retaining this record
                 source          % url of node who told us about this user
                }).

%% A Platformer node.
-record(pfnode, {id,             % an id for the node (formed using a prefix and an md5 hash of the address)
                 scheme,         % http | https
                 host,           % hostname (e.g., example.com)
                 port,           % port (e.g., 8000)
                 status,         % active | deleted
                 rating,         % 0 (worst) - 100 (best)
                 last_modified   % when last modified by node retaining this record
                }).

%% A token sent along to track queries.
-record(query_token, {uuid,      % a unique id (v4 uuid)
                      age,       % how "old" (number of hops) is this query
                      received   % when the query bearing this token was received
                     }).

%% A subset of what's in a full node record, for announcing self.
-record(nodespec, {scheme,       % http | https
                   host,         % hostname (e.g., example.com)
                   port}).       % port (e.g., 8000)


