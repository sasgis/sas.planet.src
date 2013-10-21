@echo off
cd ..\
hg log --template "{rev}" -r . > ".\Tools\revision.txt"
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
cd .\.bin
madExceptPatch.exe "SASPlanet.exe" ".\..\Tools\madexcept\SASPlanet.mes"
cd ..
cscript .\Tools\ResetVersionInfo.js
cscript .\Tools\madexcept\Clear.js
call .\Tools\ResetBuildInfo.cmd %~dp0..\
del /F /Q u_MadExcept.pas
del /F /Q ".\Tools\revision.txt"
pause