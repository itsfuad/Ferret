#!/bin/bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
ROOT_DIR=$(cd "$SCRIPT_DIR/.." && pwd)
RUNTIME_DIR="$ROOT_DIR/runtime"
LIB_DIR="$ROOT_DIR/libs"

CC=${CC:-}
if [ -z "$CC" ]; then
	if [ "$(uname -s)" = "Darwin" ] && command -v clang >/dev/null 2>&1; then
		CC=clang
	else
		CC=gcc
	fi
fi

AR=${AR:-ar}

CFLAGS=(-std=c99 -O2 -w)
if [ "$(uname -s)" = "Linux" ]; then
	CFLAGS+=(-fno-pie)
fi

mkdir -p "$LIB_DIR"
OBJ_DIR=$(mktemp -d)
trap 'rm -rf "$OBJ_DIR"' EXIT

for src in "$RUNTIME_DIR"/*.c; do
	[ -e "$src" ] || continue
	obj="$OBJ_DIR/$(basename "${src%.c}.o")"
	"$CC" "${CFLAGS[@]}" -I "$RUNTIME_DIR" -c "$src" -o "$obj"
done

"$AR" rcs "$LIB_DIR/libferret_runtime.a" "$OBJ_DIR"/*.o
if command -v ranlib >/dev/null 2>&1; then
	ranlib "$LIB_DIR/libferret_runtime.a"
fi

echo "Runtime library built: $LIB_DIR/libferret_runtime.a"
