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

     /* Seed nodes -- should be generally guaranteed to be always available. */
     seed_nodes: [ {address: 'http://0.0.0.0:8000' } ],

     /* The current userid. */
     userid: undefined,

     /* A list of known nodes. */
     nodes: [],

     /* An array used in picking the next node (see _nextNode()). */
     unused_nodes: [],

     /* To be called when creating the Platformer object. */
     init: function () {
       this.userid = localStorage.getItem('active_userid');
       this.nodes = localStorage.getItem('known_nodes');
     },

     /* To be called after the Platformer object is created and the page is ready. */
     connect: function () {
       this.updateUserids();
       this.updateKnownNodes();
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
                          pf._chooseNextId();
                          break;

                        default:
                          Platformer.log('Success uncertain.');
                        }
                      },
                      error: function (xhr, textStatus, error) {
                        switch (xhr.status) {
                        case 410:
                          Platformer.log('User is already gone; cannot be deleted.');
                          pf._chooseNextId();
                          break;

                        default:
                          Platformer.log('Unexpected error ' + xhr.status);
                        }
                      }
                     });
     },

     /* Get a new userid from a platformer node. */
     getUserid: function () {
       var pf = this;
       this._request('/user',
                     [201],
                     {type: 'POST',
                      dataType: 'json',
                      success: function (data, textStatus, xhr) {
                        Platformer._showStatus(xhr, textStatus);
                        switch (xhr.status) {
                        case 201:
                          pf.updateUserids(data.user.id);
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
      * Test whether the currently selected user is known to exist.
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
                         Platformer.log('User exists.');
                         break;
                       }
                     },
                     error: function (xhr, textStatus, error) {
                       Platformer._showStatus(xhr, textStatus);
                       switch (xhr.status) {
                       case 410:
                         Platformer.log('User has been deleted.');
                         break;
                       }
                     }
                    });
     },

     // Items (select, buttons) to hide when there's no userid selected.
     _hideItems: ['#active-userid', '#delete-userid', '#clear-userids', '#test-exists'],

     /* Update the list of known nodes. */
     updateKnownNodes: function () {
       var pf = this;
       this._request('/node/list',
                     [200],
                     {type: 'GET',
                      dataType: 'json',
                      success: function (data, textStatus, xhr) {
                        if (data == null) {
                          Platformer.log('Received null result.');
                        }
                        else {
                          data.nodes = $.map(data.nodes, function (element, index) {
                                                 return {address: element.scheme + "://" + element.host + ":" + element.port};
                                               });
                          Platformer._mergeArrays(pf.nodes, data.nodes, "address");
                          Platformer.log('Got ' + data.nodes.length + ' node(s).');
                          // Now show the status of known nodes.
                          $('#known-nodes').html('<ul></ul>');
                          $.each(pf.nodes, function (index, node) {
                                   status = pf._pingNode(node);
                                   $('#known-nodes ul').append('<li class="' + status + '">' + node.address + '</li>');
                                 });
                        }
                      }
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
      * Choose the next node to be used.
      * The optional avoid parameter can specify that we avoid
      * returning the specified nodes, returning undefined
      * if there are no remaining choices.
      */
     _nextNode: function (avoid) {
       var unused_count = this.unused_nodes.length;

       // If no nodes are known, use the seeds.
       if (!this.nodes || this.nodes.length == 0) {
         this.log('No nodes known; using seeds.');
         if (this.seed_nodes.length == 0) {
           this.log('No seed nodes configured; cannot continue.');
           throw('No seed nodes configured.');
         }
         this.nodes = this.seed_nodes.slice(0);  // .slice(0) returns a copy
       }

       // If all nodes have been used, refill the unused array.
       if (unused_count == 0) {
         this.unused_nodes = this.nodes.slice(0);  // .slice(0) returns a copy
         unused_count = this.unused_nodes.length;
       }

       // Choose a node randomly and remove it from the unused array.
       var choice = Math.floor(Math.random() * unused_count);
       node = this.unused_nodes[choice];
       this.unused_nodes.splice(choice, 1);

       // Return the url for the chosen node (avoiding the given one if necessary).
       if (avoid != undefined && ($.inArray(node, avoid) > -1)) {
         if (this.unused_nodes.length > 0) {
           return this._nextNode(avoid);
         }
         return undefined;
       }
       return node;
     },

     /* Attempt to contact the given node.  Return "ok" or "unavailable" depending on status.*/
     _pingNode: function (node) {
       var status = "unavailable";
       $.ajax({url: node.address + '/',
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

     /* Apply a given ajax request to the next node (trying additional nodes if one fails). */
     _request: function (suffix, successCode, ajaxParams, triedNodes) {
       var node = this._nextNode(triedNodes);

       // Save the original complete function.
       ajaxParams.originalComplete = (ajaxParams.originalComplete ? ajaxParams.originalComplete : ajaxParams.complete);

       // If we've tried all nodes and still nothing, call the original error function and give up.
       if (node == undefined) {
         Platformer._showMessage('Could not reach any nodes.', 'error');
         ajaxParams.originalComplete();
         ajaxParams.noNodesReachable = true;
         return;
       }

       triedNodes = (triedNodes == undefined ? [] : triedNodes);
       triedNodes.push(node);

       ajaxParams.url = node.address + suffix;
       Platformer.log('Trying ' + ajaxParams.type + ' request to ' + ajaxParams.url);

       var pf = this;

       // jQuery does not call error() all the time we want it to, and sometimes calls complete() too many times.
       ajaxParams.complete = function (xhr, textStatus) {
         if ((xhr == undefined || xhr.status === 0) && !ajaxParams.errorHandled && !ajaxParams.noNodesReachable) {
           ajaxParams.errorHandled = true;
           Platformer.log('Request did not succeed.');
           pf._request(suffix, successCode, ajaxParams, triedNodes);
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

     _chooseNextId: function () {
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

