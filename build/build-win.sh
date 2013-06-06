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

popd

pushd "$BUILD"
  echo   Build Executable
  cp "$SRC"/build/arduino.ico "$BUILD"
  "$RACO" exe --ico arduino.ico -o "$DDNAME.exe" "$SRC/plumb-gui.rkt"
  rm arduino.ico
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
  if [[ $1 = "upload" ]]; then
    scp -i ~/.ssh/small-imac-berea Plumb.zip jadudm@jadud.com:~/jadud.com/downloads/   
  fi
popd

# echo SCP Everything
# "$PSCP" "$DDNAME.zip" jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/



echo End Of Script
