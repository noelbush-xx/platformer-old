#!/usr/bin/env python
from unittest import *
import httplib, re, uuid

UUID_REGEXP = re.compile(r'[a-f\d]{8}\-'
                         r'[a-f\d]{4}\-'
                         r'4[a-f\d]{3}\-'
                         r'[89ab][a-f\d]{3}\-'
                         r'[a-f\d]{12}')

MEMO_TYPE = "user"

class MemoTests(TestCase):
    """This class contains tests related to creating, verifying and
    deleting generic memos.  It should be considered an abstract
    class, in that it is expected that subclasses will define certain
    key regexps."""


    def __init__(self, methodName='runTest', memo_type=None):
        assert memo_type is not None
        TestCase.__init__(self, methodName)
        self.memo_type = memo_type
        self._testMethodDoc = self._testMethodDoc.replace('{memo}', memo_type)
        self.id_regexp = re.compile('platformer_' + memo_type + '_' + UUID_REGEXP.pattern)
        self.json_regexp = re.compile(r'\{"' + memo_type + '":' r'\{"id":"' + self.id_regexp.pattern + '"\}\}', re.I)
        self.path_regexp = re.compile('/' + memo_type + '/' + self.id_regexp.pattern)


    # Although propagation headers do not have to be produced by a
    # client, they are allowed.  So our first tests involve validation
    # of these headers.  For all other tests, we omit them.


    def test_00_create_memo_with_headers(self):
        """Create {memo} via POST, *with* propagation headers.  Receive 201, valid JSON for {memo}, and correct Location header."""

        self.request('POST', '/' + self.memo_type + '',
                     headers = {'X-Platformer-Memo-Token': uuid.uuid4(), 'X-Platformer-Memo-Priority': '3'},
                     response_code = httplib.CREATED)

        # Check JSON response body.
        self.match_response_body(self.json_regexp)

        # Check Location header.
        self.memo_path = self.match_header('location', self.path_regexp)


    def test_00a_create_memo_bad_missing_token_header(self):
        """Omit {memo} token header when creating {memo} via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/' + self.memo_type, headers = {'X-Platformer-Memo-Priority': '3'},
                     response_code = httplib.BAD_REQUEST)


    def test_00b_create_memo_bad_missing_priority_header(self):
        """Omit {memo} priority header when creating {memo} via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/' + self.memo_type, headers = {'X-Platformer-Memo-Token': uuid.uuid4()},
                     response_code = httplib.BAD_REQUEST)


    def test_00c_create_memo_bad_noninteger_priority_header(self):
        """Pass non-integer {memo} priority header when creating {memo} via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/' + self.memo_type, headers = {'X-Platformer-Memo-Token': uuid.uuid4(),
                                                 'X-Platformer-Memo-Priority': 'three'},
                     response_code = httplib.BAD_REQUEST)


    def test_00d_create_memo_bad_negative_priority_header(self):
        """Pass negative {memo} priority header when creating {memo} via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/' + self.memo_type, headers = {'X-Platformer-Memo-Token': uuid.uuid4(),
                                                 'X-Platformer-Memo-Priority': '-2'},
                     response_code = httplib.BAD_REQUEST)


    def test_00e_create_memo_bad_toolarge_priority_header(self):
        """Pass too-large {memo} priority header when creating {memo} via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/' + self.memo_type, headers = {'X-Platformer-Memo-Token': uuid.uuid4(),
                                                 'X-Platformer-Memo-Priority': '5'},
                     response_code = httplib.BAD_REQUEST)


    def test_00f_create_memo_bad_invalid_token_header(self):
        """Pass invalid {memo} token when creating {memo} via POST. Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/' + self.memo_type, headers = {'X-Platformer-Memo-Token': uuid.uuid1(),
                                                 'X-Platformer-Memo-Priority': 'three'},
                     response_code = httplib.BAD_REQUEST)

    #
    # All the following tests omit propagation headers (the normal case).

    def test_01_create_memo(self):
        """Create {memo} via POST (no propagation headers).  Receive 201, valid JSON for {memo}, and correct Location header."""

        self.request('POST', '/' + self.memo_type, response_code = httplib.CREATED)

        # Check JSON response body.
        self.match_response_body(self.json_regexp)

        # Check Location header.
        self.memo_path = self.match_header('location', self.path_regexp)


    def test_02_verify_created_memo(self):
        """Create {memo} via POST, verify with HEAD.  Receive 200."""

        self.test_01_create_memo()
        self.request('HEAD', self.memo_path, response_code = httplib.OK, new_prep = True)


    def test_03_delete_memo(self):
        """Create {memo} via POST, then DELETE.  Receive 204."""

        self.test_01_create_memo()
        self.request('DELETE', self.memo_path, response_code = httplib.NO_CONTENT, new_prep = True)


    def test_04_verify_deleted_memo(self):
        """Create and delete {memo}, then verify deleted with HEAD.  Receive 410."""
        self.test_03_delete_memo()

        self.request('HEAD', self.memo_path, response_code = httplib.GONE, new_prep = True)


    def test_05_invalid_memo_id(self):
        """Request a {memo}_id with invalid syntax.  Receive 400."""
        self.request('HEAD', '/' + self.memo_type + '/platformer_blah_blah')
