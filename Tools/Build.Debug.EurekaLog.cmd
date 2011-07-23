@echo off
cd ..\
copy ".\Tools\eurekalog\EurekaLog.pas" ".\" /Y
cscript .\Tools\eurekalog\Clear.js
cscript .\Tools\eurekalog\Prepare.js
cscript .\Tools\CreateVersion.js
call rsvars.bat
call .\Resources\Build.Resources.cmd
msbuild SASPlanet.dproj /p:Configuration=Debug /t:rebuild
ECC32 --el_alter_exe"SASPlanet.dpr" --el_config".\Tools\eurekalog\SASPlanet.eof"
cscript .\Tools\eurekalog\Clear.js
del /F /Q EurekaLog.pas
pause