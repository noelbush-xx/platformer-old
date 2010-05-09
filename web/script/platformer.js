(function() {

  Platformer = window.Platformer = function() {
      return new Platformer.fn.init();
  };
  
  Platformer.fn = Platformer.prototype = {
  
    /* The current userid. */
    userid: null,
  
    /* A list of known servers. */
    servers: [ {host: "platformer", port: 8000} ],
    
    /* An array used in picking the next server (see nextServer()). */
    unused_servers: [],
  
    init: function () {
      this.displayUserid();
    },
    
    /* Display the current userid. */
    displayUserid: function () {
      $("#userid").text(this.userid == null ? "not set" : this.userid);
    },
  
    /* Get a new userid from a platformer server. */
    getUserid: function () {
      $.ajax({
        url: this.nextServer() + "/userid",
        dataType: "jsonp",
        success: function (data) {
          Platformer.userid = data.userid;
          Platformer.displayUserid();
        }
      });
    },
    
    /*
     * Return the next server to be used.
     * TODO: Implement non-repeating random selection.
    */
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
      return "http://" + server.host + ":" + server.port;
    }
  
  };

  Platformer.fn.init.prototype = Platformer.fn;
  
})();
Platformer = new Platformer();

$(document).ready(function () {
  Platformer.init();
});

