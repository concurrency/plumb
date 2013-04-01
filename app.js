// base64.encode, base64.decode
require('./libs/base64.js')


var express = require('express');
var app = express();

var fs = require('fs');

var sys = require('sys');
var exec = require('child_process').exec;

var pickle = require('pickle');

app.configure(function () {
  app.use(express.bodyParser());
  app.use('/ide', express.static(__dirname + '/ide'));  
});

var compileArduino = function (code) {
  fs.mkdir('sketch', function () {
    fs.mkdir('sketch/lib', function () {
      fs.mkdir('sketch/src', function () {
        fs.writeFile('sketch/src/web.ino', code, 
          function (err) {
            if (err) throw err;
            
            console.log('File saved.')
            process.chdir('sketch');
            var child = exec('ino build --arduino-dist /Applications/-Code/Arduino.app/Contents/Resources/Java/', function (error, stdout, stderr) { sys.print('stdout: ' + stdout); });
            fs.readFile('.build/environment.pickle', 
              function (err, data) {
                if (err) throw err;
                pickle.loads(data, function (orig) {
                  for (e in orig) {
                    if (orig[e][0] == 'build_dir') {
                      var build_dir = orig[e][1];
                      console.log(build_dir);
                    }
                  } 
                });
            });
          });        
      });
    });
  });
  
}

var runCompiler = function (req, resp) {
  // var code = req.body.code; 
  data = req.body;
  console.log(data);
  code = base64.decode(data.code);
  console.log(code);
  data = {};
  var hex = "";
  
  if (data.language == 'arduino') {
    console.log('Compiling for Arduino');
    hex = compileArduino(code);
  }
  
  data.result = "SUCCESS";
  data.hex = base64.encode(hex);
  
  resp.json(data);
}

app.post('/compile', runCompiler);

var THEPORT = process.env.PORT || 3000
app.listen(THEPORT);

console.log('Listening on port ' + THEPORT);