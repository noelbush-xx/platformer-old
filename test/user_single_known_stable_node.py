#!/usr/bin/env python
import unittest
import user_tests, single_known_stable_node

# This runs user tests on a single known, stable node.
class TestUserSingleKnownStableNode(single_known_stable_node.TestSingleKnownStableNode, user_tests.UserTests):
    pass

if __name__ == '__main__':
    unittest.main()
