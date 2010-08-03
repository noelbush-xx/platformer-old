#!/usr/bin/env python
from node_comm import *
import httplib, random, re, subprocess, time

NODE_COUNT = 10

class MultipleKnownStableNodes(NodeCommunicator):
    """This provides connectivity with several known, stable nodes.
    It does not include test methods."""

    def setUp(self):
        """Setup starts NODE_COUNT Platformer nodes and verifies connectivity with all of them."""

        # First be sure there aren't already any nodes running.
        subprocess.call(['../platformer', '--quiet', 'killall'])        

        # Now start the nodes we want.
        subprocess.call(['../multiple-nodes', '--count=' + str(NODE_COUNT), '--reset-db', '--quiet', 'start'])
        self.unused_nodes = []
        self.nodes = zip(['0.0.0.0'] * NODE_COUNT, range(8000, 8000 + NODE_COUNT))

        # Cycle through all clients and verify they are responding.
        for node in self.nodes:
            self.host = node[0]
            self.port = node[1]
            self.wait_for_client_ready()

        # Now let prep_client() randomly choose a client.
        self.prep_client()

    def choose_client(self):
        """This chooses the next node from a randomly shuffled list, repopulating the list first if necessary."""

        if len(self.unused_nodes) == 0:
            self.unused_nodes = list(self.nodes)
            random.shuffle(self.unused_nodes)

        # Choose (and remove) a node from the shuffled list.
        choice = self.unused_nodes.pop(0)

        self.host = choice[0]
        self.port = choice[1]

    def tearDown(self):
        """Teardown stops the Platformer nodes."""

        subprocess.call(['../multiple-nodes', '--count=' + str(NODE_COUNT), '--quiet', 'stop'])
        self.client.close()
