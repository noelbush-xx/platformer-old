# Platformer

This is very much a work in progress.  Right now there is a minimal
implementation of Platformer nodes (servers) in Erlang, a test suite in Python,
and a client in Javascript.  The initial focus has been on being able to run
multiple nodes that can keep track of one another, and being able to send
messages for entity CRUD to any node in the network and getting back a sensible
result.

## What to try/look at

* Look at the help for the `platformer` runscript:

	<pre>$ ./platformer --help
	usage: ./platformer [options] {start|stop|restart|status}
	options (specify any or none):
	-p|--port=PORT         configure the Platformer node to listen on port PORT (default: 8000)
	-r|--reset-db          reset the database on startup
	-B|--background        run in the background (no shell)
	-c|--config=NAME       read application configuration from ./priv/NAME.config (default: platformer_node)
	-L|--log-config=NAME   read logging configuration from ./priv/NAME.config (default: log-errors)
	-q|--quiet             don't print any messages about starting and stopping
	-h|--help              print this help message

	command (specify one):
	start                  start the specified Platformer node
	stop                   stop the specified Platformer node
	restart                restart the specified Platformer node
	status                 check whether the specified Platformer node is running
	connect                connect to the specified Platformer node
	killall                attempt to forcefully kill all Platformer nodes that are running</pre>

* Try running a single Platformer node (`./platformer -B start`), then visit
  `web/web-user-client.html` in a browser.  You should see the running node in
  the list, and be able to create a new userid and then verify its existence
  (woo-hoo).

* Look at the help for the `multiple-nodes` runscript:

	<pre>$ ./multiple-nodes --help
	usage: ./multiple-nodes [options] {start|stop|restart|status}
	options (specify any or none):
	-C|--count=NUMBER      how many Platformer nodes to start (default: 2)
	-s|--start=PORT        the port number to start with (default: 8000)
	-r|--reset-db          reset the database(s) on startup
	-c|--config=NAME       read application configuration from NAME.config (default: )
	-L|--log-config=NAME   read logging configuration from NAME.config (default: log-errors)
	-q|--quiet             don't print any messages about starting and stopping
	-h|--help              print this help message

	command (specify one):
	start                  start the specified Platformer nodes
	stop                   stop the specified Platformer nodes
	restart                restart the specified Platformer nodes
	status                 check whether the specified Platformer nodes are running</pre>

* Start up a few more nodes (e.g., `./multiple-nodes -C5 -s8001`).  Go back to
  the web client page, and click "Update Known Nodes".  You should see more than
  one node listed now.  Try the same tests of creating, testing existence of,
  and deleting userids.  You should see in the log console that these operations
  are happening using randomly selected nodes, and sensible behavior results.

* Run and examine the tests (in `test`) to get a feel for basic interaction with
  Platformer nodes from Python.

* Look at the (Javascript) client code (in `web/script/platformer.js`) to see
  another approach to interacting with Platformer nodes.
