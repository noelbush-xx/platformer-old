#!/usr/bin/env python
import unittest
import httplib, re, time

SERVER_TIMEOUT = 10

class NodeCommunicator(unittest.TestCase):
    """This provides a testing framework and utilities for communicating
    with nodes.  It does not implement all necessary functionality;
    subclasses should implement choose_client()."""

    def prep_client(self):
        """Close any existing client, choose another (implemented by
        subclasses), and wait for it to become available."""

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

        self.choose_client()
        self.wait_for_client_ready()

    def wait_for_client_ready(self):
        """Try to connect to the client, repeating until successful or SERVER_TIMEOUT has expired."""

        self.client = httplib.HTTPConnection(self.host, self.port, timeout=SERVER_TIMEOUT)

        # Keep trying to connect until the server responds or the
        # timeout is exceeded.
        timeout = time.time() + SERVER_TIMEOUT
        connected = False
        while time.time() < timeout:
            try:
                self.client.connect()
                connected = True
                break
            except:
                time.sleep(1)
        if not connected:
            self.fail('Unable to connect to node at ' + self.host + ':' + str(self.port) + '.')

    def request(self, method, uri, body=None, headers={}):
        """Just a simple wrapper for sending a request and capturing the
        response, so individual methods don't have to worry about when
        this gets done."""

        # Keep the method and uri so we can examine them later if needed.
        self.method = method
        self.uri = uri
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
                         '(' + self.method + ' request to ' + self.host + ':' + str(self.port) + self.uri + ')\n' +
                         'Response body: \n' + self.response_body)
        
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
                           
