#!/usr/bin/env python
from unittest import *
from user_tests import *
from single_known_stable_node import *

class TestUserSingleKnownStableNode(SingleKnownStableNode, UserTests):
    """This runs user tests on a single known, stable node."""
    pass

if __name__ == '__main__':
    suite = TestLoader().loadTestsFromTestCase(TestUserSingleKnownStableNode)
    TextTestRunner(verbosity=2).run(suite)
