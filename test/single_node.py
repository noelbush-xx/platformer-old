#!/usr/bin/env python
import unittest
import httplib, re, subprocess, time

SERVER_TIMEOUT = 2
USERID_REGEXP = re.compile(r'platformer_user_'
                           r'[a-f\d]{8}\-'
                           r'[a-f\d]{4}\-'
                           r'4[a-f\d]{3}\-'
                           r'[89ab][a-f\d]{3}\-'
                           r'[a-f\d]{12}')
USER_JSON_REGEXP = re.compile(r'\{"user":' r'\{"id":"' + USERID_REGEXP.pattern + '"\}\}', re.I)
USER_PATH_REGEXP = re.compile(r'/user/' + USERID_REGEXP.pattern)

class SingleNodeTests(unittest.TestCase):

    # Setup starts a single Platformer node and verifies connectivity with it.
    def setUp(self):
        subprocess.call(['../platformer', '--reset-db', '--background', '--quiet', 'start'], stdout=1, stderr=subprocess.STDOUT)
        self.client = httplib.HTTPConnection('0.0.0.0', 8000)

        # Keep trying to connect until the server responds or the timeout is exceeded.
        timeout = time.time() + SERVER_TIMEOUT
        connected = False
        while time.time() < timeout:
            try:
                self.client.connect()
                connected = True
                break
            except:
                pass
        if not connected:
           self.fail('Unable to connect to node.  Tests will fail.')

    def tearDown(self):
        subprocess.call(['../platformer', '--quiet', 'stop'], stdout=1, stderr=subprocess.STDOUT)
        self.client.close()

    # Create a user by POSTing to /user.
    # Verify that a 201 response is received, containing a valid JSON representation of
    # a user, and a Location header set to a path for the new user.
    def test_00_create_user(self):
        self.client.request('POST', '/user')
        response = self.client.getresponse()

        # Check status code.
        self.assertEqual(response.status, httplib.CREATED,
                         'Response status was ' + str(response.status) + ' instead of ' + str(httplib.CREATED))

        # Check JSON response body.
        data = response.read()
        self.assertTrue(re.match(USER_JSON_REGEXP, data),
                        'User json not matched by returned data: ' + data)

        # Check Location header.
        self.user_path = response.getheader('Location', '')
        self.assertNotEqual(self.user_path, '', 'Empty/non-existent Location header')
        self.assertTrue(re.match(USER_PATH_REGEXP, self.user_path),
                        'Location header was ' + self.user_path)
                           

    # Create a user, then verify that the user exists with a HEAD request.
    def test_01_verify_created_user(self):
        self.test_00_create_user()
        self.client.request('HEAD', self.user_path)
        response = self.client.getresponse()

        # Check status code.
        self.assertEqual(response.status, httplib.OK,
                         'Response status was ' + str(response.status) + ' instead of ' + str(httplib.OK))

    # Create a user, then delete it and verify deletion.
    def test_02_verify_delete_user(self):
        self.test_00_create_user()
        self.client.request('DELETE', self.user_path)
        response = self.client.getresponse()

        # Check status code.
        self.assertEqual(response.status, httplib.OK,
                         'Response status was ' + str(response.status) + ' instead of ' + str(httplib.OK))

if __name__ == '__main__':
    unittest.main()
