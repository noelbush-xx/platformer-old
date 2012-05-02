#!/usr/bin/env python
from unittest import *
import random
import subprocess
from multiple_nodes import *
from user_tests import *
from shared_fixture import *

class ChaoticNodes(MultipleNodes, SharedFixture):
    """This provides connectivity with a "space" of chaotically
    available nodes.  It does not include test methods."""

    simulator_started = False
    process = None


    def sharedSetUp(self):
        """Start up the simulator."""
        self.start_simulator()

        # Wait for the stable seed to become ready, then wait a little more time.
        self.prep_client('0.0.0.0', 8000)
        self.wait_for_client_ready()


    def sharedTearDown(self):
        """Stop the simulator."""
        self.stop_simulator()


    def start_simulator(self, node_range = (1, 10), port_range = (8000, 8150),
                        stable_seeds = [8000], wait_range = (1, 10), chaos = 50):

        # First be sure there aren't already any nodes running.
        subprocess.call(['../platformer', '--quiet', 'killall'])

        # Start the simulator with given parameters.
        self.process = subprocess.Popen(['./simulator',
                                         '--min=' + str(node_range[0]), '--max=' + str(node_range[1]),
                                         '--low-port=' + str(port_range[0]), '--high-port=' + str(port_range[1]),
                                         '--stable=' ','.join(map(lambda(i): str(i), stable_seeds)),
                                         '--min-wait=' + str(wait_range[0]), '--max-wait=' + str(wait_range[1]),
                                         '--chaos=' + str(chaos)],
                                         #'--quiet'],
                                       cwd = '..')

        node_count = port_range[1] - port_range[0]
        self.unused_nodes = []
        self.nodes = zip(['0.0.0.0'] * node_count, range(8000, 8000 + node_count))

        self.simulator_started = True


    def stop_simulator(self):
        self.process.terminate()
        self.simulator_started = False


    def setUp(self):
        self.retry = True
        if self.simulator_started:
            try:
                self.prep_client(True)
            except AttributeError:
                pass
#            self.unused_nodes = ChaoticNodes.unused_nodes


    def tearDown(self):
        if self.simulator_started:
            try:
                self.client.close()
            except AttributeError:
                pass
#            self.unused_nodes = self.unused_nodes


    def choose_node(self):
        """This chooses the next node from a randomly shuffled list, repopulating the list first if necessary."""

        self.fill_unused_nodes()

        # Choose (and remove) a node from the shuffled list.
        choice = self.unused_nodes.pop(0)

        if choice is None:
            return False

        self.host = choice[0]
        self.port = choice[1]
        return True
