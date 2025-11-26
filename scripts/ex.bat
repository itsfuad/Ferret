@echo off

rem get file path argument
set FILE_PATH=%1

rem check if file path is provided
if "%FILE_PATH%"=="" (
    echo Usage: run.bat ^<file-path^>
    exit /b 1
)

cd ..

rem check if the file exists
if not exist "%FILE_PATH%" (
    echo File not found: %FILE_PATH%
    exit /b 1
)

rem run the Ferret executable with the provided file path
bin\ferret.exe "%FILE_PATH%"

if %errorlevel% neq 0 (
    echo Execution failed.
    exit /b %errorlevel%
)

echo Execution succeeded.