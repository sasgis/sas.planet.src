@echo off
cd ..\
cscript .\Tools\CreateVersion.js
call rsvars.bat
set Configuration=Release
msbuild SASPlanet.dproj
pause