@echo off
echo Building WebAssembly module...
echo --------------------------------------------
set GOOS=js
set GOARCH=wasm
go build -v -o bin\ferret.wasm main_wasm.go
if %errorlevel% neq 0 (
    echo [FAILED] WASM build failed.
    exit /b %errorlevel%
)
echo [OK] WASM module: bin\ferret.wasm

rem Copy to bin directory as well for backup
if not exist %TARGET_DIR% mkdir %TARGET_DIR%
copy /Y bin\ferret.wasm %TARGET_DIR%\ferret.wasm >nul
echo [OK] Copied to: %TARGET_DIR%\ferret.wasm

echo.
echo ====================================
echo Build Complete!
echo ====================================
