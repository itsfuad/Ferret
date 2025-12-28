@echo off
setlocal

set SCRIPT_DIR=%~dp0
set ROOT_DIR=%SCRIPT_DIR%..
set RUNTIME_DIR=%ROOT_DIR%\runtime
set LIB_DIR=%ROOT_DIR%\libs

if not exist "%LIB_DIR%" (
    mkdir "%LIB_DIR%"
)

if "%CC%"=="" set CC=gcc
if "%AR%"=="" set AR=ar

set OBJ_DIR=%LIB_DIR%\obj
if exist "%OBJ_DIR%" (
    rmdir /s /q "%OBJ_DIR%"
)
mkdir "%OBJ_DIR%"

for %%f in ("%RUNTIME_DIR%\*.c") do (
    "%CC%" -std=c99 -O2 -w -I "%RUNTIME_DIR%" -c "%%f" -o "%OBJ_DIR%\%%~nf.o"
    if errorlevel 1 exit /b 1
)

"%AR%" rcs "%LIB_DIR%\libferret_runtime.a" "%OBJ_DIR%\*.o"
if errorlevel 1 exit /b 1

rmdir /s /q "%OBJ_DIR%"

echo Runtime library built: %LIB_DIR%\libferret_runtime.a
