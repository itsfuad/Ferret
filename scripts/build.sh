#!/bin/bash
set -euo pipefail

# Ensure Go exists
command -v go >/dev/null 2>&1

go run ./tools
go build -v -o bin/ferret
echo "Build complete."
