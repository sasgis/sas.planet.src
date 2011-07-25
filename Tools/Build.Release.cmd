@echo off
cd ..\
hg log --template "{rev}" -r . > ".\Tools\revision.txt" 
cscript .\Tools\CreateVersionInfo.js
cscript .\Tools\CreateVersion.js
cd .\Resources
call Build.Resources.cmd
cd ..\
call rsvars.bat
msbuild SASPlanet.dproj /p:Configuration=Release /t:rebuild
cscript .\Tools\ResetVersionInfo.js
pause