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
     servers: [ 'http://platformer:8000'/*,
                'http://platformer:8001'*/ ],

     /* An array used in picking the next server (see nextServer()). */
     unused_servers: [],

     init: function () {
       this.userid = localStorage.getItem('active_userid');
       this.updateUserids();
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
       $.ajax({
                url: this.nextServer() + '/userid/' + pf.userid,
                type: 'DELETE',
                complete: Platformer._showStatus
              });

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

     /* Get a new userid from a platformer server. */
     getUserid: function () {
       var pf = this;
       $.ajax({
                url: this.nextServer() + '/userid',
                type: 'POST',
                dataType: 'json',
                complete: Platformer._showStatus,
                success: function (data, status, xhr) {
                  pf.updateUserids(data.userid);
                },
                error: function (xhr, status, error) {
                  $('#messages').text('Error getting a userid: ' + status);
                }
              });
     },

     /* Return the next server to be used. */
     nextServer: function () {
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

       // Form the url for the chosen server.
       return server;
     },

     /*
      * Test whether the currently selected userid is known to exist.
      * Obviously, this should always find that it does!
      */
     testUseridExists: function () {
       // $('#messages').html('Existence not verified.');
       // var xhr = new XMLHttpRequest();
       // xhr.open('HEAD', this.nextServer() + '/userid/' + this.userid, true);
       // xhr.onreadystatechange = function () {
       //   $('#messages').html('Status: ' + xhr.status);
       // };
       // xhr.send(null);
       var pf = this;
       result = $.ajax({
                url: this.nextServer() + '/userid/' + this.userid,
                type: 'HEAD',
                complete: Platformer._showStatus
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

       // Hide the select and the delete button if there are no userids.
       if (this.userid == null) {
         $('#choose-userid').hide();
         $('#delete-userid').hide();
         $('#clear-userids').hide();
         $('#test-exists').hide();
       }
       else {
         $('#choose-userid').show();
         $('#delete-userid').show();
         $('#clear-userids').show();
         $('#text-exists').show();
       }
     },

     _showMessage: function (message, type) {
       type = type == undefined ? 'info' : type;
       $('#messages').html('<span class="' + type + '">' + message + '</span>');
     },

     _showStatus: function (xhr, textStatus) {
       Platformer._showMessage('Status: ' + xhr.status, xhr.status == 404 ? 'error' : 'info');
     },

     // HELPER FUNCTIONS

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

