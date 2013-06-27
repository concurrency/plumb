#!bash

function thebuild {
  echo Setting Up Variables
  DDNAME=$1
  SOURCE=$2
  UPLOAD=$3

  DATE=`date +%Y%m%d`
  SRC=~/git/plumb
  BUILD=$SRC/build-win
  DEST=$BUILD/$DDNAME-$DATE
  RACO=/c/Program\ Files/Racket/raco.exe
  RACKET=/c/Program\ Files/Racket/racket.exe
  ZIP=/c/Program\ Files/7-Zip/7z.exe
  PSCP=~/My\ Documents/GitHub/pscp.exe
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
  
    echo Write version file
    pushd ..
    rm -f version.rkt
    "$RACKET" build/write-version.rkt
    popd
 
    echo   Build Executable
    cp "$SRC"/build/"$ICON.ico" "$BUILD"
    "$RACO" exe --ico "$ICON.ico" -o "$DDNAME.exe" "$SRC/$SOURCE"
    rm "$ICON.ico" 
    echo   Make It Distributable
    "$RACO" distribute "$DEST" "$DDNAME.exe"
    echo Remove the original build to eliminate confusion
    rm "$BUILD/$DDNAME.exe"
  popd

  echo Copy Needed Directories
  cp -R "$SRC/client-config" "$DEST/client-config"

  mkdir -p ../completed-builds/
  cp "$DDNAME-$DATE.zip" ../completed-builds/

  pushd "$BUILD"
    echo Zip Everything
    "$ZIP" a -r "$DDNAME-$DATE.zip" "$DDNAME-$DATE"
    if [[ $UPLOAD = "upload" ]]; then
      if [[ -f ~/.ssh/small-imac-berea ]]; then
        KEY=~/.ssh/small-imac-berea
      fi
      if [[ -f ~/.ssh/big-mac-berea ]]; then
        KEY=~/.ssh/big-mac-berea
      fi

      scp -i "$KEY" "$DDNAME-$DATE".zip jadudm@jadud.com:~/jadud.com/downloads/plumb/   
      echo http://jadud.com/downloads/plumb/$DDNAME-$DATE.zip
    fi
  popd


  # echo SCP Everything
  # "$PSCP" "$DDNAME.zip" jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/

  echo End Of Script
}

thebuild Plumb ide.rkt $1
thebuild PlumbBYOE plumb-byoe.rkt $1
