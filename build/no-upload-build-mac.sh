BASE=Flow
APP=${BASE}.app
FLOWAPPCONTENTS=${APP}/Contents
ICON=arduino
VOLICON=concurrencycc-logo

rm -rf ${APP}

pushd ../server/
raco exe --gui -o ${BASE} gui.rkt
say -v Victoria "Done compiling."
mkdir -p /tmp/${BASE}-dist
raco distribute /tmp/${BASE}-dist ${APP}
rm -rf ${APP}
mv /tmp/${BASE}-dist/${APP} . 
say -v Victoria "Ready to distribute."
mv ${APP} ../build
popd

# http://hints.macworld.com/article.php?story=20051225191249727
pushd ../build/
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
mkdir -p ${FLOWAPPCONTENTS}/temp
mkdir -p ${FLOWAPPCONTENTS}/conf
touch ${FLOWAPPCONTENTS}/conf/not-found.html

say -v Victoria "Done copying."
