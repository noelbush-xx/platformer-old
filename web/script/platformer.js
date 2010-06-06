/**
 * Platformer user client code
 *
 * Copyright (c) 2010 Noel Bush <noel@platformer.org>
 * Licensed under GNU GPL v3 or later
 */
'use strict';

(function() {

   Platformer = window.Platformer = function() {
     return new Platformer.fn.init();
   };

   Platformer.fn = Platformer.prototype = {

     /* The current userid. */
     userid: undefined,

     /* A list of known servers. */
     servers: [ 'http://0.0.0.0:8000',
                'http://0.0.0.0:8001' ],

     /* An array used in picking the next server (see _nextServer()). */
     unused_servers: [],

     init: function () {
       this.userid = localStorage.getItem('active_userid');
       this.updateUserids();
     },

     /*
      * Functions to be called by the page
      */

     /* Clear the html element holding logging output. */
     clearLog: function () {
       $('#log').text('');
     },

     /* Clear all stored userids. */
     clearStoredUserids: function () {
       localStorage.removeItem('userids');
       localStorage.removeItem('active_userid');
       this.userid = undefined;
       this.updateUserids();
       this._showMessage('Cleared.');
     },

     /* Delete the current userid. */
     deleteUserid: function () {
       var pf = this;

       // Note that the browser must implement preflighting as per https://developer.mozilla.org/En/HTTP_Access_Control
       this._request('/user/' + pf.userid,
                     [204],
                     {type: 'DELETE',
                      success: function (data, textStatus, xhr) {
                        status = Platformer._showStatus(xhr, textStatus);
                        switch (status) {
                        case 204:
                          var ids = this._loadUserids();
                          var index = $.inArray(this.userid, ids);
                          if (index != -1) {
                            ids.splice(index, 1);
                          }

                          // Select the first userid in the remaining list.
                          this._setUserid(ids[0]);

                          // Save and update.
                          this._saveUserids(ids);
                          this.updateUserids();
                          break;
                        }
                      }
                     });
     },

     /* Get a new userid from a platformer server. */
     getUserid: function () {
       var pf = this;
       this._request('/user',
                     [201],
                     {type: 'POST',
                      dataType: 'json',
                      success: Platformer._showStatus,
                      success: function (data, textStatus, xhr) {
                        status = Platformer._showStatus(xhr, textStatus);
                        if (status == 201) {
                          pf.updateUserids(data.userid);
                        }
                      }
                     });
     },

     /* Append a message to the log element. */
     log: function(message) {
       $('#log').append('<p>' + message + '</p>');
     },

     /* Set the active userid. */
     setActiveUserid: function (id) {
       this.userid = id;
     },

     /*
      * Test whether the currently selected userid is known to exist.
      * It might not if it has been deleted elsewhere.
      */
     testUseridExists: function () {
       this._request('/user/' + this.userid,
                    [200],
                    {type: 'HEAD',
                     success: function (data, textStatus, xhr) {
                       Platformer._showStatus(xhr, textStatus);
                     },
                     beforeSend: function (xhr) {
                       xhr.setRequestHeader("X-Platformer-Query-Token", Math.uuid());
                       xhr.setRequestHeader("X-Platformer-Query-Age", "0");
                     }
                    });
     },

     // Items (select, buttons) to hide when there's no userid selected.
     _hideItems: ['#choose-userid', '#delete-userid', '#clear-userids', '#test-exists'],

     /*
      * Update the list of userids, selecting the active one.
      * If an argument is provided, it is appended to the current
      * list and selected, and the list is stored.
      */
     updateUserids: function (new_id) {

       // Load current list of userids and empty the select list.
       var ids = this._loadUserids();
       $('#active-userid > option').detach();

       // Add the new userid (if given).
       if (new_id != null) {
         ids.push(new_id);
         this._setUserid(new_id);
         this._saveUserids(ids);
       }

       // Build the options, selecting the current id.
       var pf = this;
       $.each(ids, function (index, value) {
                $('#active-userid').append('<option value="' + value + '"' +
                                           (value == pf.userid ? ' selected="selected"' : '') +
                                           '>' + value + '</option>');
              });

       // Hide the select and the buttons if there are no userids.
       if (this.userid == null) {
         $.each(this._hideItems, function (_, id) { $(id).hide(); });
       }
       else {
         $.each(this._hideItems, function (_, id) { $(id).show(); });
       }
     },

     /*
      * Utility functions
      */

     /*
      * Choose the next server to be used.
      * The optional avoid parameter can specify that we avoid
      * returning the specified servers, returning undefined
      * if there are no remaining choices.
      */
     _nextServer: function (avoid) {
       var unused_count = this.unused_servers.length;

       // If all servers have been used, refill the unused array.
       if (unused_count == 0) {
         this.unused_servers = this.servers.slice(0);
         unused_count = this.unused_servers.length;
       }

       // Choose a server randomly and remove it from the unused array.
       var choice = Math.floor(Math.random() * unused_count);
       server = this.unused_servers[choice];
       this.unused_servers.splice(choice, 1);

       // Return the url for the chosen server (avoiding the given one if necessary).
       if (avoid != undefined && ($.inArray(server, avoid) > -1)) {
         if (this.unused_servers.length > 0) {
           return this._nextServer(avoid);
         }
         return undefined;
       }
       return server;
     },

     /* Apply a given ajax request to the next server (trying additional servers if one fails). */
     _request: function(suffix, successCode, ajaxParams, triedServers) {
       var server = this._nextServer(triedServers);

       // Save the original complete function.
       ajaxParams.originalComplete = (ajaxParams.originalComplete ? ajaxParams.originalComplete : ajaxParams.complete);

       // If we've tried all servers and still nothing, call the original error function and give up.
       if (server == undefined) {
         Platformer._showMessage('Could not reach any servers.', 'error');
         ajaxParams.originalComplete();
         ajaxParams.noServersReachable = true;
         return;
       }

       triedServers = (triedServers == undefined ? [] : triedServers);
       triedServers.push(server);

       ajaxParams.url = server + suffix;
       Platformer.log('Trying ' + ajaxParams.type + ' request to ' + ajaxParams.url);

       var pf = this;

       // jQuery does not call error() all the time we want it to, and sometimes calls complete() too many times.
       ajaxParams.complete = function (xhr, textStatus, error) {
         if ((xhr == undefined || xhr.status === 0) && !ajaxParams.errorHandled && !ajaxParams.noServersReachable) {
           ajaxParams.errorHandled = true;
           Platformer.log('Request did not succeed.');
           pf._request(suffix, successCode, ajaxParams, triedServers);
         }
       };

       ajaxParams.errorHandled = false;
       $.ajax(ajaxParams);
     },

     _showMessage: function (message, type) {
       type = type == undefined ? 'info' : type;
       $('#messages').html('<span class="' + type + '">' + message + '</span>');
       Platformer.log(message);
     },

     _showStatus: function (xhr, textStatus) {
       var status = xhr.status;
       Platformer._showMessage('Status: ' + status, status == 404 ? 'error' : 'info');
     },

     /*
      * Storage and retrieval functions
      */

     /* Load known userids from local storage; return empty array if none defined. */
     _loadUserids: function () {
       var ids = localStorage.getItem('userids');
       if (ids != null) {
         ids = $.parseJSON(ids);
       }
       else {
         ids = [];
       }
       return ids;
     },

     /* Save the given array of userids to local storage. */
     _saveUserids: function (ids) {
       if (ids != null && ids.length > 0) {
         localStorage.setItem('userids', JSON.stringify(ids));
       }
       else {
         localStorage.removeItem('userids');
       }
     },

     /* Set the userid to the given id. */
     _setUserid: function (id) {
       this.userid = id;
       if (id != undefined) {
         localStorage.setItem('active_userid', id);
       }
       else {
         localStorage.removeItem('active_userid');
       }
     }
   };

   Platformer.fn.init.prototype = Platformer.fn;

 })();

$(document).ready(function () {
                    // Load json2.js if browser doesn't include ECMAScript 5.
                    if (!(typeof JSON === 'object' && JSON.parse)) {
                      $.getScript('script/json2.js');
                    }
                    Platformer = new Platformer();
                  });

