@echo off
cd ..\
hg log --template "{rev}" -r . > ".\Tools\revision.txt"
copy ".\Tools\eurekalog\EurekaLog.pas" ".\" /Y
cscript .\Tools\eurekalog\Clear.js
cscript .\Tools\eurekalog\Prepare.js
cscript .\Tools\CreateVersionInfo.js
cscript .\Tools\CreateVersion.js
cd .\Resources
call Build.Resources.cmd
cd ..\
call rsvars.bat
msbuild SASPlanet.dproj /p:Configuration=Debug /t:rebuild
ECC32 --el_alter_exe"SASPlanet.dpr" --el_config".\Tools\eurekalog\SASPlanet.eof"
cscript .\Tools\ResetVersionInfo.js
cscript .\Tools\eurekalog\Clear.js
del /F /Q EurekaLog.pas
pause