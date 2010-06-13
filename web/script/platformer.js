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

     /* Seed servers -- should be generally guaranteed to be always available. */
     seed_servers: [ {address: 'http://0.0.0.0:8000' } ],

     /* The current userid. */
     userid: undefined,

     /* A list of known servers. */
     servers: [],

     /* An array used in picking the next server (see _nextServer()). */
     unused_servers: [],

     /* To be called when creating the Platformer object. */
     init: function () {
       this.userid = localStorage.getItem('active_userid');
       this.servers = localStorage.getItem('known_servers');
     },

     /* To be called after the Platformer object is created and the page is ready. */
     connect: function () {
       this.updateUserids();
       this.updateKnownServers();
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
       this._showMessage('Cleared all stored userids.');
     },

     /* Delete the current userid. */
     deleteUserid: function () {
       var pf = this;

       // Note that the browser must implement preflighting as per https://developer.mozilla.org/En/HTTP_Access_Control
       this._request('/user/' + pf.userid,
                     [204],
                     {type: 'DELETE',
                      success: function (data, textStatus, xhr) {
                        Platformer._showStatus(xhr, textStatus);
                        switch (xhr.status) {
                        case 204:
                          Platformer.log('Successfully deleted.');
                          var ids = pf._loadUserids();
                          var index = $.inArray(pf.userid, ids);
                          if (index != -1) {
                            ids.splice(index, 1);
                          }

                          // Select the first userid in the remaining list.
                          pf._setUserid(ids[0]);

                          // Save and update.
                          pf._saveUserids(ids);
                          pf.updateUserids();
                          break;

                        default:
                          Platformer.log('Success uncertain.');
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
                        Platformer._showStatus(xhr, textStatus);
                        switch (xhr.status) {
                        case 201:
                          pf.updateUserids(data.userid);
                          Platformer.log('Successfully retrieved new userid.');
                          break;
                        }
                      }
                     });
     },

     /* Append a message to the log element. */
     log: function(message) {
       $('#log').append('<p>' + message + '</p>').scrollTop($('#log').attr('scrollHeight'));
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
                       switch (xhr.status) {
                       case 200:
                         Platformer.log('Userid exists.');
                         break;
                       }
                     },
                     error: function (xhr, textStatus, error) {
                       Platformer._showStatus(xhr, textStatus);
                       switch (xhr.status) {
                       case 410:
                         Platformer.log('Userid has been deleted.');
                         break;
                       }
                     },
                     beforeSend: function (xhr) {
                       xhr.setRequestHeader("X-Platformer-Query-Token", Math.uuid());
                       xhr.setRequestHeader("X-Platformer-Query-Age", "0");
                     }
                    });
     },

     // Items (select, buttons) to hide when there's no userid selected.
     _hideItems: ['#active-userid', '#delete-userid', '#clear-userids', '#test-exists'],

     /* Update the list of known servers. */
     updateKnownServers: function () {
       var pf = this;
       this._request('/server/list',
                     [200],
                     {type: 'GET',
                      dataType: 'json',
                      success: function (data, textStatus, xhr) {
                        Platformer._mergeArrays(pf.servers, data.servers, "address");
                        Platformer.log('Got ' + data.servers.length + ' server(s).');
                      }
                     });

       // Now show the status of known servers.
       $('#known-servers').html('<ul></ul>');
       var pf = this;
       $.each(this.servers, function (index, server) {
                status = pf._pingServer(server);
                $('#known-servers ul').append('<li class="' + status + '">' + server.address + '</li>');
              });
     },

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

       // If no servers are known, use the seeds.
       if (!this.servers || this.servers.length == 0) {
         this.log('No servers known; using seeds.');
         if (this.seed_servers.length == 0) {
           this.log('No seed servers configured; cannot continue.');
           throw('No seed servers configured.');
         }
         this.servers = this.seed_servers.slice(0);  // .slice(0) returns a copy
       }

       // If all servers have been used, refill the unused array.
       if (unused_count == 0) {
         this.unused_servers = this.servers.slice(0);  // .slice(0) returns a copy
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

     /* Attempt to contact the given server.  Return "ok" or "unavailable" depending on status.*/
     _pingServer: function (server) {
       var status = "unavailable";
       $.ajax({url: server.address + '/',
               async: false,
               timeout: 200,
               type: 'OPTIONS',
               success: function (data, textStatus, xhr) {
                 if (xhr && xhr.status && typeof xhr.status == "number" && xhr.status == 200) {
                   status = "available";
                 }
               }
              });
       return status;
     },

     /* Apply a given ajax request to the next server (trying additional servers if one fails). */
     _request: function (suffix, successCode, ajaxParams, triedServers) {
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

       ajaxParams.url = server.address + suffix;
       Platformer.log('Trying ' + ajaxParams.type + ' request to ' + ajaxParams.url);

       var pf = this;

       // jQuery does not call error() all the time we want it to, and sometimes calls complete() too many times.
       ajaxParams.complete = function (xhr, textStatus) {
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

     /*
      * Merge two arrays, optionally avoiding duplicates.
      * If dupeCriteria is undefined, just does the same thing as
      * jQuery's $.merge().  But dupeCriterion can specify what to
      * compare in the two arrays, as follows:
      * - a value of true says to avoid copying absolute duplicates
      * - a value of "key_name" says to avoid copying objects where "key_name" has the same value
      */
     _mergeArrays: function (array0, array1, dupeCriterion) {
       if (dupeCriterion == undefined) {
         return $.merge(array0, array1);
       }
       if (dupeCriterion === true) {
         $.each(array1, function (index1, value1) {
                  if ($.inArray(value1, array0) === -1) {
                    array0.push(value1);
                  }
                });
       }
       else if (typeof dupeCriterion == "string") {
         $.each(array1, function (index1, value1) {
                  if (!$.isPlainObject(value1)) {
                    array0.push(value1);
                  }
                  else {
                    var found = false;
                    $.each(array0, function (index0, value0) {
                             if ($.isPlainObject(value0) && dupeCriterion in value0) {
                               if (value0[dupeCriterion] === value1[dupeCriterion]) {
                                 found = true;
                               }
                             }
                           });
                    if (!found) {
                      array0.push(value1);
                    }
                  }
                });
       }
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
                    Platformer.connect();
                  });

