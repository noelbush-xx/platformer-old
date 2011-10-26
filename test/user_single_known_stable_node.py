#!/usr/bin/env python
from unittest import *
from memo_tests import *
from single_known_stable_node import *

class TestUserSingleKnownStableNode(SingleKnownStableNode, MemoTests):
    """This runs user tests on a single known, stable node."""

    def __init__(self, methodName='runTest'):
        MemoTests.__init__(self, methodName, 'user')

if __name__ == '__main__':
    suite = TestLoader().loadTestsFromTestCase(TestUserSingleKnownStableNode)
    TextTestRunner(verbosity=2).run(suite)
