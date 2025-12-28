#!/usr/bin/env sh
set -eu

REPO="${FERRET_REPO:-itsfuad/Ferret}"
BASE_URL="${FERRET_RELEASE_BASE:-https://github.com/${REPO}/releases/latest/download}"
INSTALL_DIR="${FERRET_INSTALL_DIR:-$HOME/.ferret}"
BIN_DIR="${INSTALL_DIR}/bin"

os="$(uname -s)"
case "$os" in
  Linux) os="linux" ;;
  Darwin) os="darwin" ;;
  *) echo "Unsupported OS: $os" >&2; exit 1 ;;
esac

arch="$(uname -m)"
case "$arch" in
  x86_64|amd64) arch="amd64" ;;
  aarch64|arm64) arch="arm64" ;;
  *) echo "Unsupported arch: $arch" >&2; exit 1 ;;
esac

archive="ferret-${os}-${arch}.tar.gz"
url="${BASE_URL}/${archive}"

tmp="$(mktemp -d 2>/dev/null || mktemp -d -t ferret)"
cleanup() { rm -rf "$tmp"; }
trap cleanup EXIT

if command -v curl >/dev/null 2>&1; then
  curl -fsSL "$url" -o "$tmp/$archive"
elif command -v wget >/dev/null 2>&1; then
  wget -qO "$tmp/$archive" "$url"
else
  echo "curl or wget is required to download Ferret" >&2
  exit 1
fi

mkdir -p "$INSTALL_DIR"
rm -rf "$INSTALL_DIR/bin" "$INSTALL_DIR/libs"
tar -xzf "$tmp/$archive" -C "$INSTALL_DIR"

if [ ! -d "$BIN_DIR" ]; then
  echo "Missing bin/ in install dir: $BIN_DIR" >&2
  exit 1
fi

echo "Installed to $INSTALL_DIR"
echo "Add to PATH:"
echo "  export PATH=\"${BIN_DIR}:\$PATH\""
