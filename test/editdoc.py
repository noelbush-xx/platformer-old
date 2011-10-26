#!/usr/bin/env python
from memo_tests import *
import inspect

for name in MemoTests.__dict__:
    item = getattr(MemoTests, name)
    if inspect.ismethod(item):
        print item.__doc__
        item.__doc__ = 'foo'
        print item.__doc__
