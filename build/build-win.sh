#!bash

echo Setting Up Variables

SRC=~/My\ Documents/GitHub/flow
BUILD=$SRC/build
DDNAME=Flow
DEST=$SRC/build/$DDNAME
RACO=/c/Program\ Files/Racket/raco.exe
ZIP=/c/Program\ Files/7-Zip/7z.exe
PSCP=~/My\ Documents/GitHub/pscp.exe

echo Remove the Destination If It Exists
pushd "$BUILD"
  rm -rf "$DDNAME"
  echo   Remove The Zipfile Too
  rm -rf "$DDNAME.zip"
 
  echo   Making Destination Directories
  mkdir "$DEST"

  echo   Make Temp Directory In Destination
  mkdir "$DEST/temp"

popd

pushd "$BUILD"
  echo   Build Executable
  "$RACO" exe -o "$DDNAME.exe" "$SRC/server/gui.rkt"
  echo   Make It Distributable
  "$RACO" distribute "$DEST" "$DDNAME.exe"
popd

echo Copy Needed Directories
cp -R "$SRC/bin" "$DEST/bin"
cp -R "$SRC/interface" "$DEST/interface"
cp -R "$SRC/occam" "$DEST/occam"
cp -R "$SRC/config" "$DEST/config"

echo Zip Everything
"$ZIP" a -r "$DDNAME.zip" "$DDNAME"

echo SCP Everything
"$PSCP" "$DDNAME.zip" jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/

echo End Of Script
