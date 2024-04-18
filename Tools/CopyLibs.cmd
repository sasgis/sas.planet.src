@echo off

set mingw=c:\msys64\mingw32\bin\
set lib=.\..\.bin\lib32\

set PATH=%mingw%;%PATH%

set dll_mingw=7z.dll imagequant.dll libgeotiff.dll libminizip-ng-1.dll
set dll_all=libfreeimage-3.dll %dll_mingw%

for %%i in (%dll_mingw%) do (
  copy /Y %mingw%%%i %lib%%%i
)

for %%i in (%dll_all%) do (
  python ./dllfetch/dllfetch.py %lib:\=/%%%i --dir=%mingw:\=/% --target-dir=%lib:\=/% --ignore=libcurl-4.dll
)

pause
