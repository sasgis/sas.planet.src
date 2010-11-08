@echo off
cscript CreateVersion.js
call rsvars.bat
set Configuration=Release
c:\Windows\Microsoft.NET\Framework\v2.0.50727\MSBuild.exe SASPlanet.dproj
pause