var express = require('express');
var app = express();

var top = function (req, res) {
  var body = 'The Top';
  res.setHeader('Content-Type', 'text/plain');
  res.setHeader('Content-Length', body.length);
  res.end(body);
}

app.get('/', top);

app.get('/hello.txt', function(req, res){
  var body = 'Hello World';
  res.setHeader('Content-Type', 'text/plain');
  res.setHeader('Content-Length', body.length);
  res.end(body);
});

app.use('/ide', express.static(__dirname + '/ide'));

var THEPORT = process.env.PORT || 3000
app.listen(THEPORT);

console.log('Listening on port ' + THEPORT);