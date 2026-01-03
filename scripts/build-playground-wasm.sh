#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WEBSITE_PUBLIC="$ROOT/../website/public"

echo "Building Go WASM compiler..."
GOCACHE="$ROOT/.gocache" GOOS=js GOARCH=wasm go build -o "$ROOT/bin/ferret.wasm" "$ROOT/main_wasm.go"

if [ -d "$WEBSITE_PUBLIC" ]; then
  cp "$ROOT/bin/ferret.wasm" "$WEBSITE_PUBLIC/ferret2.wasm"
  echo "Copied wasm compiler to $WEBSITE_PUBLIC/ferret2.wasm"
else
  echo "Website public folder not found: $WEBSITE_PUBLIC"
  echo "Built wasm compiler at $ROOT/bin/ferret.wasm"
fi
