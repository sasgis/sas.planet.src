@echo off

set mingw=c:\msys64\mingw64\bin\
set lib=.\..\.bin\win64\lib64\

set PATH=%mingw%;%PATH%

set dll=7z.dll libsqlite3-0.dll imagequant.dll libgeotiff.dll libminizip-ng-1.dll libcurl-4.dll libfreeimage-3.dll

for %%i in (%dll%) do (
  echo Copying: %%i
  copy /Y "%mingw%%%i" "%lib%%%i"
)

for %%i in (%dll%) do (
  python ./dllfetch/dllfetch.py "%lib:\=/%%%i" --dir "%mingw:\=/%" --target-dir "%lib:\=/%"
)

pause
