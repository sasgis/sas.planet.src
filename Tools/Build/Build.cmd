@echo off

set build_config=%1
set build_platform=%2
set build_type=%3
set build_em=%4

set script_path=%~dp0.
set src_path=%script_path%\..\..
set req_path=%script_path%\..\..\..
set res_path=%script_path%\..\..\Resources
set exe_name=%src_path%\.bin\%build_platform%\SASPlanet.exe

cd "%src_path%"

if "%build_em%"=="EL" (
	copy "%script_path%\eurekalog\u_EurekaLog.pas" ".\" /Y
	cscript "%script_path%\eurekalog\Clear.js"
	cscript "%script_path%\eurekalog\Prepare.js"
) else (
	if "%build_em%"=="ME" (
		copy "%script_path%\madexcept\u_MadExcept.pas" ".\" /Y
		cscript "%script_path%\madexcept\Clear.js"
		cscript "%script_path%\madexcept\Prepare.js"
	)
)

git rev-list --count master > "%script_path%\revision.txt"
cscript "%script_path%\CreateVersionInfo.js"
call "%script_path%\CreateBuildInfo.cmd" "%build_type%" "%src_path%" "%req_path%"

cd "%res_path%"
call Build.Resources.cmd

cd "%src_path%"
call rsvars.bat
msbuild SASPlanet.dproj "/p:config=%build_config%" "/p:platform=%build_platform%" "/t:rebuild"

if "%build_em%"=="EL" (
	ecc32.exe "--el_alter_exe=SASPlanet.dpr;%exe_name%" "--el_config=%script_path%\eurekalog\SASPlanet.eof"
	cscript "%script_path%\eurekalog\Clear.js"
	del /F /Q u_EurekaLog.pas
) else (
	if "%build_em%"=="ME" (
		madExceptPatch.exe "%exe_name%" "%script_path%\madexcept\SASPlanet.mes"
		cscript "%script_path%\madexcept\Clear.js"
		del /F /Q u_MadExcept.pas
	)
)

cscript "%script_path%\ResetVersionInfo.js"
call "%script_path%\ResetBuildInfo.cmd" "%src_path%"
del /F /Q "%script_path%\revision.txt"

cd "%res_path%"
call Build.Resources.cmd

pause