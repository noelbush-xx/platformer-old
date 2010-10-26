#!/usr/bin/env python
from node_comm import *
import httplib, random, re, subprocess, time

NODE_COUNT = 10

class MultipleKnownStableNodes(NodeCommunicator):
    """This provides connectivity with several known, stable nodes.
    It does not include test methods."""

    node_count = NODE_COUNT

    def start_nodes(self, count):
        """Starts a specified number of Platformer nodes and verifies
        connectivity with all of them."""

        MultipleKnownStableNodes.node_count = count

        # First be sure there aren't already any nodes running.
        subprocess.call(['../platformer', '--quiet', 'killall'])        

        # Now start the nodes we want and set up the node-tracking variables.
        subprocess.call(['../multiple-nodes', '--count=' + str(count), '--reset-db', '--quiet', 'start'])
        self.unused_nodes = []
        self.nodes = zip(['0.0.0.0'] * count, range(8000, 8000 + count))

        # Cycle through all nodes and verify they are responding.
        for node in self.nodes:
            self.prep_client(node[0], node[1])
            result = self.wait_for_client_ready()

        # Now let prep_client() randomly choose a node.  (Note that
        # prep_client() is implemented by NodeCommunicator, but it
        # requires us to implement choose_node().
        self.prep_client()

    def choose_node(self):
        """This chooses the next node from a randomly shuffled list, repopulating the list first if necessary."""

        if len(self.unused_nodes) == 0:
            self.unused_nodes = list(self.nodes)
            random.shuffle(self.unused_nodes)

        # Choose (and remove) a node from the shuffled list.
        choice = self.unused_nodes.pop(0)

        self.host = choice[0]
        self.port = choice[1]

    def stop_nodes(self):
        """Stops all of the Platformer nodes we started."""

        subprocess.call(['../multiple-nodes', '--count=' + str(MultipleKnownStableNodes.node_count), '--quiet', 'stop'])
        self.client.close()
