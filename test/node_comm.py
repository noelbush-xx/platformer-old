#!/usr/bin/env python
from unittest import *
import httplib, re, time

SERVER_TIMEOUT = 10

class NodeCommunicator(TestCase):
    """This provides a testing framework and utilities for
    communicating with nodes.  It does not implement all necessary
    functionality; subclasses should implement choose_node().  It
    extends TestCase because it uses some assertion and fail
    methods of that class."""

    def prep_client(self, host=None, port=None):
        """Close the client if open, then if a host and port are
        specified, set up a new client to connect with the indicated
        node; if no host and port are specified, then choose them
        (using a subclass's implementation of choose_node()), and wait
        for the corresponding node to become available.  If retry is
        set to True, will try different nodes if one is not available.
        Returns a bool indicating whether the client prep was
        successful."""

        try:
            self.client.close()
        except:
            # There was no client yet; no problem.
            pass

        # Also delete the current response object.
        try:
            del self.response
        except:
            # Wasn't defined; no problem.
            pass

        # We will store all request info for backtracing errors.
        if not hasattr(self, 'requests'):
            self.requests = []

        connected = False
        
        # We may have been passed a (first) node to try.
        if host is not None and port is not None:
            self.host = host
            self.port = port
            node_chosen = True
        else:
            node_chosen = False

        # This loop will be exited by one of the return statements
        # (as long as choose_node() works correctly).
        while True:
            # Try getting a node if one has not been chosen.
            if not node_chosen:
                node_chosen = self.choose_node()
            # Still no node chosen means there are no more available to choose.
            if not node_chosen:
                return False
            # Otherwise, try to connect; return True if successful.
            self.client = httplib.HTTPConnection(self.host, self.port, timeout=SERVER_TIMEOUT)
            if self.wait_for_client_ready():
                return True
            # Otherwise unset node_chosen and let the loop go again.
            node_chosen = False

        # This is here in case we mess up the logic of the above while loop.
        self.fail('Logic broken in NodeCommunicator.prep_client().')

    def wait_for_client_ready(self):
        """Try to connect to the client, repeating until successful or
        SERVER_TIMEOUT has expired.  Unless self.retry is set to True, a
        failure to connect will cause a test failure.  If self.retry is
        False, the function will return a boolean indicating whether
        the connection was successful."""

        timeout = time.time() + SERVER_TIMEOUT
        connected = False
        while time.time() < timeout:
            try:
                self.client.connect()
                connected = True
                break
            except:
                time.sleep(1)

        if not connected and not self.retry:
            self.fail('Unable to connect to node at ' + self.host + ':' + str(self.port) + '.')

        return connected

    def request(self, method, uri, body=None, headers={}, response_code=None, new_prep=False):
        """Performs a specified request, captures the response, and
        (optionally) verifies that the response code matches an
        expectation.  The bool new_prep says whether to prepare the
        client anew before sending the request."""

        # Prepare the client (if directed).
        if new_prep:
            proceed = self.prep_client()
        else:
            proceed = True

        while proceed:
            # We will decide below whether to propagate test failures.
            try:
                # Store request data for backtrace.
                self.requests.append([self.host, self.port, method, uri, body, headers])

                self.client.request(method, uri, body, headers)
                try:
                    self.response = self.client.getresponse()
                except httplib.BadStatusLine:
                    self.fail('Error reading response from request to ' + self.host + ':' + str(self.port) + '.')
                else:
                    self.response_body = self.response.read()
                    
                    if response_code is not None:
                        self.assertEqual(self.response.status, response_code,
                                         'Response status was ' + str(self.response.status) +
                                         ' instead of ' + str(response_code) + '\n' +
                                         self.request_backtrace() +
                                         'Final response body: \n' + self.response_body)
            except AssertionError as e:
                if self.retry:
                    proceed = self.prep_client()
                else:
                    proceed = False
                if not proceed:
                    raise e
            # If we got here, the request was successful.
            proceed = False
        
    def match_response_body(self, regexp):
        """Assume request().  Match response body against given regexp, and
        provide explanatory message if not."""

        self.assertTrue(re.match(regexp, self.response_body),
                        'Expected pattern not matched by response body: ' + self.response_body)

    def match_header(self, headername, regexp):
        """Assume request().  Test existence of given headername, then test
        that its contents match the given regexp.  Provide explanatory
        message if not.  Otherwise, return header contents."""

        self.response_headers = dict(self.response.getheaders())
        self.assertTrue(headername in self.response_headers,
                        'Header "' + headername + '" not found in headers (' + str(self.response_headers.keys()))
        header_value = self.response_headers[headername]
        self.assertTrue(re.match(regexp, header_value),
                        'Expected pattern not matched by ' + headername + ' value: ' + header_value)
        return header_value

    def request_backtrace(self):
        result = 'Backtrace:\n'
        for request in self.requests:
            result += ' - %s to %s:%s%s\n' % (request[2], request[0], request[1], request[3])
        return result
