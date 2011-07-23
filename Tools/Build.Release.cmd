@echo off
cd ..\
cscript .\Tools\CreateVersion.js
call rsvars.bat
call .\Resources\Build.Resources.cmd
msbuild SASPlanet.dproj /p:Configuration=Release /t:rebuild
pause