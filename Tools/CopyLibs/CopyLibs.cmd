@echo off

set mingw=c:\msys64\mingw32\bin\
set lib=.\..\..\.bin\win32\lib32\

set PATH=%mingw%;%PATH%

set dll_mingw=libsqlite3-0.dll
set dll_all=imagequant.dll libproj-25.dll libgeotiff.dll libminizip-ng-1.dll libfreeimage-3.dll %dll_mingw%

for %%i in (%dll_mingw%) do (
  echo Copying: %%i
  copy /Y "%mingw%%%i" "%lib%%%i"
)

for %%i in (%dll_all%) do (
  python ./dllfetch/dllfetch.py "%lib:\=/%%%i" --dir "%mingw:\=/%" --target-dir "%lib:\=/%" --ignore libcurl-4.dll 7z.dll imagequant.dll
)

pause
