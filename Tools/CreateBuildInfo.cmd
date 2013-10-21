
set BuildType=%1
set SrcPath="%2"
set ReqPath="%3"
set CSV="%2Resources\BuildInfo\BuildInfo.csv"

echo %BuildType%
echo %SrcPath%
echo %ReqPath%
echo %CSV%

rem Get current date in "yymmdd"-format
for /f "skip=1" %%x in ('wmic os get localdatetime') do if not defined BuildDate set BuildDate=%%x
set sas_date=%BuildDate:~2,2%%BuildDate:~4,2%%BuildDate:~6,2%
echo %sas_date%

rem Get sources revision and node hash
cd %SrcPath%

for /f "delims=" %%a in ('hg log --template {rev} -r .') do @set SrcRev=%%a
IF NOT ERRORLEVEL 0 goto error
echo %SrcRev%

for /f "delims=" %%a in ('hg log --template {node} -r .') do @set SrcNode=%%a
IF NOT ERRORLEVEL 0 goto error
echo %SrcNode%

rem Get requires revision and node hash
cd %ReqPath%

for /f "delims=" %%a in ('hg log --template {rev} -r .') do @set ReqRev=%%a
IF NOT ERRORLEVEL 0 goto error
echo %ReqRev%

for /f "delims=" %%a in ('hg log --template {node} -r .') do @set ReqNode=%%a
IF NOT ERRORLEVEL 0 goto error
echo %ReqNode%

rem Save build info to csv-file
echo 1,%sas_date%,%BuildType%,%SrcRev%,%SrcNode%,%ReqRev%,%ReqNode%, > %CSV%

goto end

:error
echo "Error!"
goto end

:end
cd %SrcPath%
