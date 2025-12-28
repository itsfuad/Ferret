echo "Building WebAssembly module..."
echo "------------------------------------"
GOOS=js GOARCH=wasm go build -v -o "bin/ferret.wasm" main_wasm.go
if [ $? -ne 0 ]; then
    echo "[FAILED] WASM build failed."
    exit 1
fi
echo "[OK] WASM module: bin/ferret.wasm"