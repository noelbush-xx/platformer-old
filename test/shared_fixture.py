#!/usr/bin/env python

class SharedFixture():

    def test_00000_setUp(self):
        """Start up the simulator."""
        self.sharedSetUp()

    def test_99999_tearDown(self):
        """Stop the simulator."""
        self.sharedTearDown()
