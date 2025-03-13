@echo off

:: Check if the current directory is not "compiler", then change to it
for %%I in ("%CD%") do if /I not "%%~nxI"=="compiler" (
    cd compiler
)

:: Clear the screen
cls

echo Running tests...

:: Run the tests
go test ./...

:: check if the tests passed
if errorlevel 1 (
    echo Tests failed
) else (
    echo Tests passed
)
