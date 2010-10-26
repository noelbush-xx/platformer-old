#!/usr/bin/env python
from node_comm import *
import subprocess, time

class SingleKnownStableNode(NodeCommunicator):
    """This provides connectivity with a single known, stable node.
    It does not include test methods."""

    def setUp(self):
        """Setup starts a single Platformer node and verifies connectivity with it."""

        # First be sure there isn't already a node running.
        subprocess.call(['../platformer', '--quiet', 'killall'])        

        # Now start the node we want.
        subprocess.call(['../platformer', '--reset-db', '--background', '--quiet', 'start'])
        self.prep_client()

    def choose_node(self):
        """Since these tests are for only one node, this method sets the default host and port."""

        self.host = '0.0.0.0'
        self.port = 8000

    def tearDown(self):
        """Teardown stops the Platformer node."""

        subprocess.call(['../platformer', '--quiet', 'stop'])
        self.client.close()

