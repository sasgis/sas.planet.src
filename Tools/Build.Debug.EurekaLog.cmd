@echo off
cd ..\
git rev-list --count master > ".\Tools\revision.txt"
copy ".\Tools\eurekalog\u_EurekaLog.pas" ".\" /Y
cscript .\Tools\eurekalog\Clear.js
cscript .\Tools\eurekalog\Prepare.js
cscript .\Tools\CreateVersionInfo.js
call .\Tools\CreateBuildInfo.cmd "Custom (EL)" %~dp0..\ %~dp0..\..\
cd .\Resources
call Build.Resources.cmd
cd ..\
call rsvars.bat
msbuild SASPlanet.dproj /p:config=Debug /p:platform=Win32 /t:rebuild
ecc32.exe --el_alter_exe"SASPlanet.dpr" --el_config".\Tools\eurekalog\SASPlanet.eof"
cscript .\Tools\ResetVersionInfo.js
cscript .\Tools\eurekalog\Clear.js
call .\Tools\ResetBuildInfo.cmd %~dp0..\
del /F /Q u_EurekaLog.pas
del /F /Q ".\Tools\revision.txt"
cd .\Resources
call Build.Resources.cmd
pause