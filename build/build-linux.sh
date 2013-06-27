#!/bin/bash

echo Setting Up Variables

DATE=`date +%Y%m%d`
SRC=~/git/plumb
BUILD=$SRC/build-linux
DDNAME=Plumb
DEST=$BUILD/$DDNAME-$DATE
RACO=raco
ZIP=tar
PSCP=scp
SOURCE=ide.rkt
ICON=concurrencycc-logo

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
  cp "$SRC"/build/${ICON}.ico "$BUILD"
  "$RACO" exe --ico ${ICON}.ico -o "$DDNAME" "$SRC/$SOURCE"
  rm ${ICON}.ico
  echo   Make It Distributable
  "$RACO" distribute "$DEST" "$DDNAME"
  echo Remove the original build to eliminate confusion
  rm "$BUILD/$DDNAME"
popd

echo Copy Needed Directories
cp -R "$SRC/client-config" "$DEST/client-config"

pushd "$BUILD"
  echo Zip Everything
  "$ZIP" cvzf "$DDNAME-$DATE.tar.gz" "$DDNAME-$DATE"
  if [[ $1 = "upload" ]]; then
    if [[ -f ~/.ssh/small-imac-berea ]]; then
      KEY=~/.ssh/small-imac-berea
    fi
    if [[ -f ~/.ssh/big-mac-berea ]]; then
      KEY=~/.ssh/big-mac-berea
    fi

    scp -i "$KEY" "$DDNAME-$DATE".tar.gz jadudm@jadud.com:~/jadud.com/downloads/   
    echo http://jadud.com/downloads/$DDNAME-$DATE.tar.gz
  fi
popd


# echo SCP Everything
# "$PSCP" "$DDNAME.zip" jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/



echo End Of Script
