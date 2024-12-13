@echo off
cd ..\
git rev-list --count master > ".\Tools\revision.txt" 
cscript .\Tools\CreateVersionInfo.js
call .\Tools\CreateBuildInfo.cmd "Stable" %~dp0..\ %~dp0..\..\
cd .\Resources
call Build.Resources.cmd
cd ..\
call rsvars.bat
msbuild SASPlanet.dproj /p:config=Release /p:platform=Win32 /t:rebuild
cscript .\Tools\ResetVersionInfo.js
call .\Tools\ResetBuildInfo.cmd %~dp0..\
del /F /Q ".\Tools\revision.txt"
cd .\Resources
call Build.Resources.cmd
pause