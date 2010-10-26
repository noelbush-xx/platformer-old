#!/usr/bin/env python
from unittest import *
from chaotic_nodes import *
from user_tests import *

class TestUserChaoticNodes(ChaoticNodes, UserTests):
    """This runs user tests on simulated environments of "chaotic"
    node availability."""
   
if __name__ == '__main__':
    suite = TestLoader().loadTestsFromTestCase(TestUserChaoticNodes)
    TextTestRunner(verbosity=2).run(suite)
