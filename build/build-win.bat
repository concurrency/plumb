@ECHO OFF

echo. Setting up variables

set src=C:\Documents and Settings\Administrator\My Documents\GitHub\flow
set build=%src%\build
set ddname=Flow
set dest=%src%\build\%ddname%
set raco="C:\Program Files\Racket\raco.exe"
set zip="C:\Program Files\7-Zip\7z.exe"

REM REMOVE THE DESTINATION IF IT EXISTS
cd "%build%"
rm -rf %ddname%
REM REMOVE THE ZIPFILE TOO
rm -rf %ddname%.zip

REM MAKE DESTINATION DIRECTORY
echo. Making destination directories.
mkdir "%dest%"

REM MAKE TEMP DIRECTORY IN DESTINATION
mkdir "%dest%\temp"

REM BUILD EXECUTABLE
echo. Building executable.
cd %build%
%raco% exe -o %ddname%.exe "%src%\server\gui.rkt"

REM MAKE IT DISTRIBUTABLE
echo. Making executable distributable.
%raco% distribute "%dest%" %ddname%.exe

REM COPY NEEDED DIRECTORIES
call:xcopy_sd bin
call:xcopy_sd interface
call:xcopy_sd occam
call:xcopy_sd config

REM ZIP EVERYTHING
echo. Zipping things up.
%zip% a -r %ddname%.zip %ddname%

echo. Consider running ssh-agent and ssh-add.
echo. See URL noted in script.
REM http://stackoverflow.com/questions/3669001/getting-ssh-agent-to-work-with-git-run-from-windows-command-shell

echo. Doing SCP.
scp -i id_rsa %ddname%.zip jadudm@transterpreter.org:/srv/www/org/transterpreter.download/files/flow/


REM End of script
GOTO:EOF

:xcopy_sd
  echo. Copying %1
  xcopy /S /F /Y /I "%src%\%1" "%dest%\%1"
GOTO:EOF