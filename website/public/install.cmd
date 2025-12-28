@echo off
setlocal

set "BASE_URL=https://ferret.brainbird.org"
if not "%FERRET_SCRIPT_BASE%"=="" set "BASE_URL=%FERRET_SCRIPT_BASE%"

powershell -NoProfile -ExecutionPolicy Bypass -Command "irm -useb %BASE_URL%/install.ps1 | iex"
if %errorlevel% neq 0 (
    echo Install failed.
    exit /b %errorlevel%
)

endlocal
