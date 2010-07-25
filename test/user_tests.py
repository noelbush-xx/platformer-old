#!/usr/bin/env python
import unittest
import httplib, re, uuid

USERID_REGEXP = re.compile(r'platformer_user_'
                           r'[a-f\d]{8}\-'
                           r'[a-f\d]{4}\-'
                           r'4[a-f\d]{3}\-'
                           r'[89ab][a-f\d]{3}\-'
                           r'[a-f\d]{12}')
USER_JSON_REGEXP = re.compile(r'\{"user":' r'\{"id":"' + USERID_REGEXP.pattern + '"\}\}', re.I)
USER_PATH_REGEXP = re.compile(r'/user/' + USERID_REGEXP.pattern)

class UserTests(unittest.TestCase):
    """This class contains tests related to creating, verifying and
    deleting users.  It does not include setup or teardown code, so
    subclasses can test different node configurations."""

    #
    # Although propagation headers do not have to be produced by a
    # client, they are allowed.  So our first tests involve validation
    # of these headers.  For all other tests, we omit them.

    def test_00_create_user_with_headers(self):
        """Create user via POST, *with* propagation headers.  Receive 201, valid JSON for user, and correct Location header."""

        self.request('POST', '/user', headers = {'X-Platformer-Memo-Token': uuid.uuid4(), 'X-Platformer-Memo-Priority': '3'})

        # Check status code.
        self.verify_status_code(httplib.CREATED)

        # Check JSON response body.
        self.match_response_body(USER_JSON_REGEXP)

        # Check Location header.
        self.user_path = self.match_header('location', USER_PATH_REGEXP)

    def test_00a_create_user_bad_missing_token_header(self):
        """Omit memo token header when creating user via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/user', headers = {'X-Platformer-Memo-Priority': '3'})

        # Check that status code is 400 BAD REQUEST.
        self.verify_status_code(httplib.BAD_REQUEST)

    def test_00b_create_user_bad_missing_priority_header(self):
        """Omit memo priority header when creating user via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/user', headers = {'X-Platformer-Memo-Token': uuid.uuid4()})

        # Check that status code is 400 BAD REQUEST.
        self.verify_status_code(httplib.BAD_REQUEST)

    def test_00c_create_user_bad_noninteger_priority_header(self):
        """Pass non-integer memo priority header when creating user via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/user', headers = {'X-Platformer-Memo-Token': uuid.uuid4(),
                                                 'X-Platformer-Memo-Priority': 'three'})

        # Check that status code is 400 BAD REQUEST.
        self.verify_status_code(httplib.BAD_REQUEST)

    def test_00d_create_user_bad_negative_priority_header(self):
        """Pass negative memo priority header when creating user via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/user', headers = {'X-Platformer-Memo-Token': uuid.uuid4(),
                                                 'X-Platformer-Memo-Priority': '-2'})

        # Check that status code is 400 BAD REQUEST.
        self.verify_status_code(httplib.BAD_REQUEST)

    def test_00e_create_user_bad_toolarge_priority_header(self):
        """Pass too-large memo priority header when creating user via POST.  Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/user', headers = {'X-Platformer-Memo-Token': uuid.uuid4(),
                                                 'X-Platformer-Memo-Priority': '5'})

        # Check that status code is 400 BAD REQUEST.
        self.verify_status_code(httplib.BAD_REQUEST)

    def test_00f_create_user_bad_invalid_token_header(self):
        """Pass invalid memo token when creating user via POST. Receive 400."""
        #TODO: Match response text too (?)

        self.request('POST', '/user', headers = {'X-Platformer-Memo-Token': uuid.uuid1(),
                                                 'X-Platformer-Memo-Priority': 'three'})

        # Check that status code is 400 BAD REQUEST.
        self.verify_status_code(httplib.BAD_REQUEST)

    #
    # All the following tests omit propagation headers (the normal case).

    def test_01_create_user(self):
        """Create user via POST (no propagation headers).  Receive 201, valid JSON for user, and correct Location header."""

        self.request('POST', '/user')

        # Check status code.
        self.verify_status_code(httplib.CREATED)

        # Check JSON response body.
        self.match_response_body(USER_JSON_REGEXP)

        # Check Location header.
        self.user_path = self.match_header('location', USER_PATH_REGEXP)

    def test_02_verify_created_user(self):
        """Create user via POST, verify with HEAD.  Receive 200."""

        self.test_01_create_user()
        self.prep_client()
        self.request('HEAD', self.user_path)

        # Check status code.
        self.verify_status_code(httplib.OK)

    def test_03_delete_user(self):
        """Create user via POST, then DELETE.  Receive 204."""

        self.test_01_create_user()
        self.prep_client()
        self.request('DELETE', self.user_path)

        # Check status code.
        self.verify_status_code(httplib.NO_CONTENT)

    def test_04_verify_deleted_user(self):
        """Create and delete user, then verify deleted with HEAD.  Receive 410."""
        self.test_03_delete_user()
        self.prep_client()
        self.request('HEAD', self.user_path)

        # Check status code.
        self.verify_status_code(httplib.GONE)
