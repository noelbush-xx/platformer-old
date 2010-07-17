#!/usr/bin/env python
import node_comm
import subprocess, time

# This provides connectivity with a single known, stable node.  It does not include test methods.
class TestSingleKnownStableNode(node_comm.NodeCommunicator):

    def setUp(self):
        """Setup starts a single Platformer node and verifies connectivity with it."""

        # First be sure there isn't already a node running.
        subprocess.call(['../platformer', '--quiet', 'killall'])        

        # Now start the node we want.
        subprocess.call(['../platformer', '--reset-db', '--background', '--quiet', 'start'])
        self.prep_client()

    def choose_client(self):
        """Since these tests are for only one node, this method sets the default host and port."""

        self.host = '0.0.0.0'
        self.port = 8000

    def tearDown(self):
        """Teardown stops the Platformer node."""

        subprocess.call(['../platformer', '--quiet', 'stop'])
        self.client.close()
