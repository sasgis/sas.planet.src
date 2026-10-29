@echo off

powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0UnitsSort.ps1" %*

echo.

pause
