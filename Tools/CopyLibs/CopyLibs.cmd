@echo off

powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0CopyLibs.ps1" %*

echo.

pause