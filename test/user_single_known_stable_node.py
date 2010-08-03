#!/usr/bin/env python
import unittest
from user_tests import *
from single_known_stable_node import *

# This runs user tests on a single known, stable node.
class TestUserSingleKnownStableNode(SingleKnownStableNode, UserTests):
    pass

if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(TestUserSingleKnownStableNode)
    unittest.TextTestRunner(verbosity=2).run(suite)
