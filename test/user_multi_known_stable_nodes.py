#!/usr/bin/env python
import unittest
from user_tests import *
from multiple_known_stable_nodes import *

# This runs user tests on multiple known, stable nodes.
class TestUserSingleKnownStableNode(MultipleKnownStableNodes, UserTests):
    pass

if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(TestUserSingleKnownStableNode)
    unittest.TextTestRunner(verbosity=2).run(suite)
