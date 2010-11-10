#!/usr/bin/env python
from node_comm import *
import random

class MultipleNodes(NodeCommunicator):
    """Provides functions common to all multi-node communications."""

    def fill_unused_nodes(self):
        self.unused_nodes = list(self.nodes)
        random.shuffle(self.unused_nodes)
