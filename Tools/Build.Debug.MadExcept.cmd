@echo off
cd ..\
git rev-list --count master > ".\Tools\revision.txt"
copy ".\Tools\madexcept\u_MadExcept.pas" ".\" /Y
cscript .\Tools\madexcept\Clear.js
cscript .\Tools\madexcept\Prepare.js
cscript .\Tools\CreateVersionInfo.js
call .\Tools\CreateBuildInfo.cmd "Custom (ME)" %~dp0..\ %~dp0..\..\sas.requires
cd .\Resources
call Build.Resources.cmd
cd ..\
call rsvars.bat
msbuild SASPlanet.dproj /p:Configuration=Debug /t:rebuild
madExceptPatch.exe ".\.bin\win32\SASPlanet.exe" ".\Tools\madexcept\SASPlanet.mes"
cscript .\Tools\ResetVersionInfo.js
cscript .\Tools\madexcept\Clear.js
call .\Tools\ResetBuildInfo.cmd %~dp0..\
del /F /Q u_MadExcept.pas
del /F /Q ".\Tools\revision.txt"
cd .\Resources
call Build.Resources.cmd
pause