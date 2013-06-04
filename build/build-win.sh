#!bash

echo Setting Up Variables

SRC=~/git/plumb
BUILD=$SRC/build-win
DDNAME=Plumb
DEST=$BUILD/$DDNAME
RACO=/c/Program\ Files/Racket/raco.exe
ZIP=/c/Program\ Files/7-Zip/7z.exe
PSCP=~/My\ Documents/GitHub/pscp.exe

pushd "$SRC"
  rm -rf "$BUILD"
popd

pushd "$SRC"
  mkdir "$BUILD"
  mkdir -p "$DEST"
popd

pushd "$BUILD"
 
  echo   Making Destination Directories
  mkdir "$DEST"

  echo   Make Temp Directory In Destination
  mkdir "$DEST/temp"

popd

pushd "$BUILD"
  echo   Build Executable
  "$RACO" exe -o "$DDNAME.exe" "$SRC/plumb-gui.rkt"
  echo   Make It Distributable
  "$RACO" distribute "$DEST" "$DDNAME.exe"
  echo Remove the original build to eliminate confusion
  rm "$BUILD/$DDNAME.exe"
popd

echo Copy Needed Directories
cp -R "$SRC/client-config" "$DEST/client-config"

pushd "$BUILD"
  echo Zip Everything
  "$ZIP" a -r "$DDNAME.zip" "$DDNAME"
popd

# echo SCP Everything
# "$PSCP" "$DDNAME.zip" jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/

echo End Of Script
