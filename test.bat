@echo off

:: Check if the current directory is not "compiler", then change to it
for %%I in ("%CD%") do if /I not "%%~nxI"=="compiler" (
    cd compiler
)

:: Clear the screen
cls

:: Run the tests
go test -v ./...