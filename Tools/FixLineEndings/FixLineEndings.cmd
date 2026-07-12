@echo off

set "script_path=%~dp0."
set "root_path=%script_path%\..\.."

for %%d in (Src Benchmark Includes Placeholders Test) do (
    powershell -ExecutionPolicy Bypass -File .\FixLineEndings.ps1 -Path "%root_path%\%%d"
    echo.
)

pause