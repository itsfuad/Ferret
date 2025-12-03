#!/bin/bash

echo "===================================="
echo "Building Ferret Compiler (All Targets)"
echo "===================================="

# Navigate to project root
cd "$(dirname "$0")/.." || exit 1

# Set target directory
TARGET_DIR="bin"

# Create target directory if it doesn't exist
if [ ! -d "$TARGET_DIR" ]; then
    echo "Creating bin directory..."
    mkdir -p "$TARGET_DIR"
fi

echo ""
echo "[1/2] Building native executable..."
echo "------------------------------------"
go build -v -o "$TARGET_DIR/ferret" main.go
if [ $? -ne 0 ]; then
    echo "[FAILED] Native build failed."
    exit 1
fi
echo "[OK] Native executable: $TARGET_DIR/ferret"

echo ""
echo "[2/2] Building WebAssembly module..."
echo "------------------------------------"
GOOS=js GOARCH=wasm go build -v -o "website/public/ferret.wasm" main_wasm.go
if [ $? -ne 0 ]; then
    echo "[FAILED] WASM build failed."
    exit 1
fi
echo "[OK] WASM module: website/public/ferret.wasm"

# Copy to bin directory as well for backup
mkdir -p "$TARGET_DIR"
cp website/public/ferret.wasm "$TARGET_DIR/ferret.wasm"
echo "[OK] Copied to: $TARGET_DIR/ferret.wasm"

echo ""
echo "===================================="
echo "Build Complete!"
echo "===================================="
echo "Native: $TARGET_DIR/ferret"
echo "WASM:   website/public/ferret.wasm"
echo "        $TARGET_DIR/ferret.wasm (backup)"
echo "===================================="
