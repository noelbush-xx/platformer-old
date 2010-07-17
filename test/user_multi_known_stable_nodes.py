#!/usr/bin/env python
import unittest
import user_tests, multiple_known_stable_nodes

# This runs user tests on multiple known, stable nodes.
class TestUserSingleKnownStableNode(multiple_known_stable_nodes.TestMultipleKnownStableNodes, user_tests.UserTests):
    pass

if __name__ == '__main__':
    unittest.main()
