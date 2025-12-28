@echo off
setlocal enabledelayedexpansion

REM Fail on first error
call :run_or_fail go run ./tools
call :run_or_fail go build -v -o bin\ferret.exe

echo Build complete.
exit /b 0

:run_or_fail
%*
if errorlevel 1 (
    echo Command failed: %*
    exit /b 1
)
exit /b 0
