#!bash

echo Setting Up Variables

DATE=`date +%Y%m%d`
SRC=~/git/plumb
BUILD=$SRC/build-win
DDNAME=Plumb
DEST=$BUILD/$DDNAME-$DATE
RACO=/c/Program\ Files/Racket/raco.exe
ZIP=/c/Program\ Files/7-Zip/7z.exe
PSCP=~/My\ Documents/GitHub/pscp.exe
SOURCE=ide.rkt

pushd "$SRC"
  rm -rf "$BUILD"
popd

echo   Making Destination Directories

pushd "$SRC"
  mkdir "$BUILD"
popd

pushd "$BUILD"
  mkdir "$DEST"
popd

pushd "$BUILD"
  echo   Build Executable
  cp "$SRC"/build/arduino.ico "$BUILD"
  "$RACO" exe --ico arduino.ico -o "$DDNAME.exe" "$SRC/$SOURCE"
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
  "$ZIP" a -r "$DDNAME-$DATE.zip" "$DDNAME-$DATE"
  if [[ $1 = "upload" ]]; then
    if [[ -f ~/.ssh/small-imac-berea ]]; then
      KEY=~/.ssh/small-imac/berea
    fi
    if [[ -f ~/.ssh/big-mac-berea ]]; then
      KEY=~/.ssh/big-mac-berea
    fi

    scp -i "$KEY" "$DDNAME-$DATE".zip jadudm@jadud.com:~/jadud.com/downloads/   
    echo http://jadud.com/downloads/$DDNAME-$DATE.zip
  fi
popd


# echo SCP Everything
# "$PSCP" "$DDNAME.zip" jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/



echo End Of Script
