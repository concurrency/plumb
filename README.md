jupiter
=======

A browser-based environment for editing occam-pi. And...

* A stub server for uploading bytecode to the Arduino
* A TVM compiled to Javascript for directly executing occam-pi in the browser.

Reason: Eliminate shipping native IDEs and toolchains.

## Step 1: Arduino C++

Proof-of-concept: compile C++ for the Arduino, ship .hex, upload to AVR.

## Step 2: occam-pi

1. Write occam-pi in the browser.
2. Ship to a (sane, Linux) server. Compile.
3. Ship back bytecode.
4. Send bytecode to (local) stub. (server.rkt)
5. Upload to Arduino.

## Step 3: TVM in the Browser

1. Compile TVM with Emcscripten (https://github.com/kripken/emscripten)
2. Write wrapper for browser.
3. User server-side compiler for generating bytecode.
4. Run bytecode directly in browser.

## Possibilities

* Compile kroc with emscripten; run compiler in browser?
* Support novice programmer research on Arduino C++/occam-pi.
* Add visualization components (http://www.graphdracula.net/de/)

### References
* http://stackoverflow.com/questions/11584061/automatically-adjust-height-to-contents-in-ace-cloud9-editor
