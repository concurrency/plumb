BASE=Flow
DEST=../${BASE}
SRC=../server
rm -rf ${DEST}
mkdir -p ${DEST}

pushd ${SRC}
  echo Building the Flow executable.
  raco exe --gui -o ${BASE} gui.rkt
  echo Sleeping
  sleep 2
  echo Readying for Distribution
  raco distribute ${DEST} ${BASE}
  echo Sleeping
  sleep 2
  echo Setting executable mode.
  chmod 755 ${DEST}/bin/${BASE}
popd
 
echo Moving files around
sleep 2
cp -R ../bin/unix/ ${DEST}/bin/unix/
cp -R ../config/ ${DEST}/config/
cp -R ../interface/ ${DEST}/interface/
cp -R ../occam/ ${DEST}/occam/
mkdir -p ${DEST}/temp
# Hopefully eliminating the "not found" errors
# that happen... for reasons unknown.
mkdir -p ${DEST}/conf
touch ${DEST}/conf/not-found.html

echo Going up one level
pushd ..
  echo Zipping.
  tar cvzf Flow.tgz Flow/

  echo If the first command line argument was "upload"...
  if [ "$1" = "upload" ]; then
        echo Uploading
	scp -i ~/.ssh/jadudm-rsa ${BASE}.tgz jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/
  fi
popd


