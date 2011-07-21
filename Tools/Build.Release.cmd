@echo off
cd ..\
cscript .\Tools\CreateVersion.js
call rsvars.bat
msbuild SASPlanet.dproj /p:Configuration=Release /t:rebuild
pause