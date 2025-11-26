@echo off
echo ====================================
echo Building Ferret Compiler (All Targets)
echo ====================================

rem Save current directory and navigate to project root
set SCRIPT_DIR=%~dp0
cd /d "%SCRIPT_DIR%\.."

rem Set target directory
set TARGET_DIR=bin

rem Create target directory if it doesn't exist
if not exist %TARGET_DIR% (
    echo Creating bin directory...
    mkdir %TARGET_DIR%
)

echo.
echo [1/2] Building native executable (Windows)...
echo --------------------------------------------
go build -v -o %TARGET_DIR%\ferret.exe main.go
if %errorlevel% neq 0 (
    echo [FAILED] Native build failed.
    exit /b %errorlevel%
)
echo [OK] Native executable: %TARGET_DIR%\ferret.exe

echo.
echo [2/2] Building WebAssembly module...
echo --------------------------------------------
set GOOS=js
set GOARCH=wasm
go build -v -o website\public\ferret.wasm main_wasm.go
if %errorlevel% neq 0 (
    echo [FAILED] WASM build failed.
    exit /b %errorlevel%
)
echo [OK] WASM module: website\public\ferret.wasm

rem Copy to bin directory as well for backup
if not exist %TARGET_DIR% mkdir %TARGET_DIR%
copy /Y website\public\ferret.wasm %TARGET_DIR%\ferret.wasm >nul
echo [OK] Copied to: %TARGET_DIR%\ferret.wasm

echo.
echo ====================================
echo Build Complete!
echo ====================================
echo Native: %TARGET_DIR%\ferret.exe
echo WASM:   website\public\ferret.wasm
echo        %TARGET_DIR%\ferret.wasm (backup)
echo ====================================
