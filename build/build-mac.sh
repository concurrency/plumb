BASE=Flow
APP=${BASE}.app
FLOWAPPCONTENTS=${APP}/Contents
ICON=arduino
VOLICON=concurrencycc-logo
BUILD=build-mac

pushd ../
  rm -rf ${BUILD}
  mkdir -p ${BUILD}
popd

pushd ../  
  raco exe --gui -o ${BASE} plumb-gui.rkt
  say -v Victoria "Done compiling."
  mkdir -p /tmp/${BASE}-dist
  raco distribute /tmp/${BASE}-dist ${APP}
  rm -rf ${APP}
  mv /tmp/${BASE}-dist/${APP} . 
  say -v Victoria "Ready to distribute."
  mv ${APP} ${BUILD}
popd

exit

# http://hints.macworld.com/article.php?story=20051225191249727
pushd ../${BUILD}
  rm ${FLOWAPPCONTENTS}/Resources/Starter.icns
  convert -scale 256x256 ${ICON}.png ${ICON}-256.png
  sips -s format icns ${ICON}-256.png --out ${FLOWAPPCONTENTS}/Resources/Starter.icns
  convert -scale 256x256 ${VOLICON}.png ${VOLICON}-256.png
  sips -s format icns ${VOLICON}-256.png --out ${VOLICON}-256.icns
popd


mkdir -p ${FLOWAPPCONTENTS}/bin/macosx/
cp -R ../bin/macosx/ ${FLOWAPPCONTENTS}/bin/macosx/
cp -R ../config/ ${FLOWAPPCONTENTS}/config/
cp -R ../interface/ ${FLOWAPPCONTENTS}/interface/
cp -R ../occam/ ${FLOWAPPCONTENTS}/occam/
# This eliminates "file not found" errors for PLT Racket
mkdir -p ${FLOWAPPCONTENTS}/conf
touch ${FLOWAPPCONTENTS}/conf/not-found.html
mkdir -p ${FLOWAPPCONTENTS}/temp

say -v Victoria "Done copying."

# Remove the DMG if it exists
rm -rf ${BASE}.dmg

# Unmount the drive, if it is mounted
if [ -d /Volumes/Flow ]; then
	hdiutil eject /Volumes/Flow
	sleep 2
fi

./create-dmg/create-dmg \
	--background ${VOLICON}.png \
	--icon-size 128 \
	--icon ${APP} 220 200 \
	--volicon ${VOLICON}-256.icns \
	${BASE}.dmg ${APP}

say -v Victoria "Done making disk image."

say -v Victoria "Reticulating splines."

if [ "${USER}" == "jadudm" ]; then
	if [ -e ${BASE}.dmg ]; then
		scp -i ~/.ssh/id_rsa ${BASE}.dmg jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/
		echo Done with upload.
	fi
fi
say -v Victoria "All done."
