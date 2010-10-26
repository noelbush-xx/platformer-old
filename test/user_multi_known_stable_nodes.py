#!/usr/bin/env python
from unittest import *
from user_tests import *
from multiple_known_stable_nodes import *

class TestUserMultipleKnownStableNode(MultipleKnownStableNodes, UserTests):
    """This runs user tests on multiple known, stable nodes.  For each
    test, the full set of nodes is reset and newly started."""
    
    def setUp(self):
        self.start_nodes(10)

    def tearDown(self):
        self.stop_nodes()

if __name__ == '__main__':
    suite = TestLoader().loadTestsFromTestCase(TestUserMultipleKnownStableNode)
    TextTestRunner(verbosity=2).run(suite)
