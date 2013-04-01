// base64.encode, base64.decode
require('./libs/base64.js')


var express = require('express');
var app = express();

app.configure(function () {
  app.use(express.bodyParser());
  app.use('/ide', express.static(__dirname + '/ide'));  
});

var runCompiler = function (req, resp) {
  // var code = req.body.code; 
  data = req.body;
  console.log(data);
  console.log(base64.decode(data.code));
  
  data = {};
  data.result = "SUCCESS";
  resp.json(data);
}

app.post('/compile', runCompiler);

var THEPORT = process.env.PORT || 3000
app.listen(THEPORT);

console.log('Listening on port ' + THEPORT);