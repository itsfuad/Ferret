#!/bin/bash

# Build script for Ferret compiler
# Builds both CLI and WASM versions

set -e

echo "🔨 Building Ferret Compiler..."

# Build CLI version
echo "📦 Building CLI version..."
go build -o ferret main.go
echo "✓ CLI build complete: ./ferret"

# Build WASM version
echo "🌐 Building WASM version..."
GOOS=js GOARCH=wasm go build -o website/public/ferret.wasm main_wasm.go
WASM_SIZE=$(ls -lh website/public/ferret.wasm | awk '{print $5}')
echo "✓ WASM build complete: website/public/ferret.wasm ($WASM_SIZE)"

echo ""
echo "✅ All builds completed successfully!"
echo ""
echo "To run the CLI:"
echo "  ./ferret <file.fer>"
echo ""
echo "To test the playground:"
echo "  cd website && npm run dev"
