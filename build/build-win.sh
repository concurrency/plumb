#!bash

echo Setting Up Variables

SRC=../
BUILD=$SRC/build-win
DDNAME=Plumb
DEST=$SRC/$BUILD/$DDNAME
RACO=/c/Program\ Files/Racket/raco.exe
ZIP=/c/Program\ Files/7-Zip/7z.exe
PSCP=~/My\ Documents/GitHub/pscp.exe

pushd "$SRC"
  rm -rf "$BUILD"
popd

pushd "$SRC"
  mkdir "$BUILD"
popd

pushd "$BUILD"
 
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
cp -R "$SRC/client-config" "$DEST/client-config"

echo Zip Everything
"$ZIP" a -r "$DDNAME.zip" "$DDNAME"

# echo SCP Everything
# "$PSCP" "$DDNAME.zip" jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/

echo End Of Script
