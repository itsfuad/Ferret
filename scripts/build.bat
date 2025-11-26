@echo off

echo Building Ferret...

rem set target directory

set TARGET_DIR=bin

rem create target directory if it doesn't exist

if not exist %TARGET_DIR% (
    mkdir %TARGET_DIR%
)

rem build the project
cd ..
go build -v -o %TARGET_DIR%\ferret.exe main.go
if %errorlevel% neq 0 (
    echo Build failed.
    exit /b %errorlevel%
)

echo Build succeeded. Output: %TARGET_DIR%\ferret.exe