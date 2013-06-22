function announce(){
  echo $1
  say -v Victoria $1
}

function thebuild() {
  DATE=`date +%Y%m%d`
  APP=${BASE}.app
  PLUMBCONTENTS=${APP}/Contents
  ICON=omer-icon
  VOLICON=concurrencycc-logo
  BUILD=build-mac
  DMGNAME=${BASE}-${DATE}
  BASE=$1
  SOURCE=$2

pushd ../
  rm -rf ${BUILD}
  mkdir -p ${BUILD}
popd

pushd ../  
  echo "Writing version file."
  rm -f version.rkt
  racket build/write-version.rkt
  announce "Compiling ${BASE}"
  raco exe --gui -o ${BASE} ${SOURCE}
  announce "Done compiling."
  mkdir -p /tmp/${BASE}-dist
  announce "Prepping distribution."
  raco distribute /tmp/${BASE}-dist ${APP}
  rm -rf ${APP}
  mv /tmp/${BASE}-dist/${APP} . 
  announce "Ready to distribute."
  mv ${APP} ${BUILD}
popd

cp ${VOLICON}.png ../${BUILD}/
cp ${ICON}.png ../${BUILD}

# http://hints.macworld.com/article.php?story=20051225191249727
pushd ../${BUILD}
  announce "Playing with icons."
  rm ${PLUMBCONTENTS}/Resources/Starter.icns
  convert -scale 256x256 ${ICON}.png ${ICON}-256.png
  sips -s format icns ${ICON}-256.png --out ${PLUMBCONTENTS}/Resources/Starter.icns
  convert -scale 256x256 ${VOLICON}.png ${VOLICON}-256.png
  sips -s format icns ${VOLICON}-256.png --out ${VOLICON}-256.icns
  announce "Done with icon wrangling."
  # rm *.png

  announce "Copying stuffs"
  mkdir -p ${PLUMBCONTENTS}/client-config/
  cp -R ../client-config ${PLUMBCONTENTS}/
  
  # This eliminates "file not found" errors for PLT Racket
  mkdir -p ${PLUMBCONTENTS}/conf
  touch ${PLUMBCONTENTS}/conf/not-found.html
  # mkdir -p ${PLUMBCONTENTS}/temp
  announce "Done copying."
  # Remove the DMG if it exists
  rm -rf ${DMGNAME}.dmg

  # Unmount the drive, if it is mounted
  if [ -d /Volumes/Flow ]; then
	  hdiutil eject /Volumes/Flow
	  sleep 2
  fi

  ../build/create-dmg/create-dmg \
	  --background ${VOLICON}.png \
    --icon-size 128 \
	  --icon ${APP} 220 200 \
	  --volicon ${VOLICON}-256.icns \
	  ${DMGNAME}.dmg ${APP}

  announce "Done making disk image."

  announce "Reticulating splines."
  # Clean up. Leave only the DMG
  rm *.png
  rm *.icns
  rm -rf ${APP}
  if [[ $1 = "upload" ]]; then
    if [[ -f ~/.ssh/small-imac-berea ]]; then
      KEY=~/.ssh/small-imac-berea
    fi
    if [[ -f ~/.ssh/big-mac-berea ]]; then
      KEY=~/.ssh/big-mac-berea
    fi
    scp -i "$KEY" ${DMGNAME}.dmg jadudm@jadud.com:~/jadud.com/downloads/   

    echo http://jadud.com/downloads/${DMGNAME}.dmg
  fi
  
popd

announce "All done."
}

announce "Building the IDE"
thebuild Plumb ide.rkt

announce "Building the BYOE"
thebuild PlumbBYOE plumb-byoe.rkt
