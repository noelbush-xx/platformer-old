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

    def prep_client(self, retry=False, host=None, port=None):
        """Close the client if open, then if a host and port are
        specified, set up a new client to connect with the indicated
        node; if no host and port are specified, then choose them
        (using a subclass's implementation of choose_node()), and wait
        for the corresponding node to become available.  If retry is
        set to True, will try different nodes if one is not available."""

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

        connected = False
        
        if host is not None and port is not None:
            self.host = host
            self.port = port
            specific_node = True
        else:
            specific_node = False

        # We will store all request info for backtracing errors.
        if not hasattr(self, 'requests'):
            self.requests = []

        while not connected:
            if not specific_node:
                self.choose_node()
            self.client = httplib.HTTPConnection(self.host, self.port, timeout=SERVER_TIMEOUT)
            connected = self.wait_for_client_ready(not retry)
            if not connected and specific_node:
                break

    def wait_for_client_ready(self, fail=True):
        """Try to connect to the client, repeating until successful or
        SERVER_TIMEOUT has expired.  Unless fail is set to False, a
        failure to connect will cause a test failure.  If fail is
        True, the function will return a boolean indicating whether
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

        if not connected and fail:
            self.fail('Unable to connect to node at ' + self.host + ':' + str(self.port) + '.')

        return connected

    def request(self, method, uri, body=None, headers={}):
        """Just a simple wrapper for sending a request and capturing the
        response, so individual methods don't have to worry about when
        this gets done."""

        # Store request data for backtrace.
        self.requests.append([self.host, self.port, method, uri, body, headers])

        self.client.request(method, uri, body, headers)
        try:
            self.response = self.client.getresponse()
        except httplib.BadStatusLine:
            self.fail('Error reading response from request to ' + self.host + ':' + str(self.port) + '.')
        else:
            self.response_body = self.response.read()
           
    def verify_status_code(self, code):
        """Assume that a request has already been sent to the client and
        the response has been read by request().  Check that the
        response code matches the given value; if it does not, print a
        message explaining the error."""

        self.assertEqual(self.response.status, code,
                         'Response status was ' + str(self.response.status) + ' instead of ' + str(code) + '\n' +
                         self.request_backtrace() +
                         'Final response body: \n' + self.response_body)
        
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
