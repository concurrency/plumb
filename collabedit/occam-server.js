#!/usr/bin/env node

// This is a simple example sharejs server which hosts the sharejs
// examples in examples/.
//
// It demonstrates a few techniques to get different application behaviour.

require('coffee-script');
var express = require('express'),
  sharejs = require('/home/reynoldsm/cloud9/ShareJS/src'),
  hat = require('hat').rack(32, 36);

var argv = require('optimist').
  usage("Usage: $0 [-p portnum]").
  default('p', 8000).
  alias('p', 'port').
  argv;

var server = express();
server.use(express.static(__dirname + '/collabedit'));

var options = {
  db: {type: 'none'},
  browserChannel: {cors: '*'},
  auth: function(client, action) {
    // This auth handler rejects any ops bound for docs starting with 'readonly'.
    if (action.name === 'submit op' && action.docName.match(/^readonly/)) {
      action.reject();
    } else {
      action.accept();
    }
  }
};

// Lets try and enable redis persistance if redis is installed...
try {
  require('redis');
  options.db = {type: 'redis'};
} catch (e) {}

console.log("occam example server v" + sharejs.version);
console.log("Options: ", options);


server.get('/session/:id', function(req, res){
  var body = 'DOCNAME';
  body = req.params.id;
});


var port = argv.p;

// Attach the sharejs REST and Socket.io interfaces to the server
sharejs.server.attach(server, options);

server.listen(port);
console.log("occam running at http://localhost:" + port);

process.title = 'sharejs-occam'
process.on('uncaughtException', function (err) {
  console.error('An error has occurred. Please file a ticket here: https://github.com/josephg/ShareJS/issues');
  console.error('Version ' + sharejs.version + ': ' + err.stack);
});
