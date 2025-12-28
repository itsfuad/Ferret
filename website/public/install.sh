#!/usr/bin/env sh
set -eu

REPO="${FERRET_REPO:-itsfuad/Ferret}"
BASE_URL="${FERRET_RELEASE_BASE:-https://github.com/${REPO}/releases/latest/download}"
TERMUX_PREFIX="${PREFIX:-}"
IS_TERMUX=0
if [ -n "${TERMUX_VERSION-}" ]; then
  IS_TERMUX=1
elif [ -n "${TERMUX_PREFIX}" ] && [ "${TERMUX_PREFIX}" = "/data/data/com.termux/files/usr" ]; then
  IS_TERMUX=1
fi
if [ "${IS_TERMUX}" -eq 1 ] && [ -z "${FERRET_ALLOW_TERMUX_RELEASE-}" ]; then
  echo "Termux detected. Prebuilt releases are not available."
  echo "Use: curl -fsSL https://ferret.brainbird.org/install-termux.sh | sh"
  exit 1
fi

if [ -n "${FERRET_INSTALL_DIR-}" ]; then
  INSTALL_DIR="${FERRET_INSTALL_DIR}"
elif [ "${IS_TERMUX}" -eq 1 ] && [ -n "${TERMUX_PREFIX}" ]; then
  INSTALL_DIR="${TERMUX_PREFIX}"
else
  INSTALL_DIR="${HOME}/.ferret"
fi

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
if [ "${IS_TERMUX}" -eq 1 ]; then
  echo "Termux detected. If this build does not run, you may need a Termux-specific release."
fi

add_path_line() {
  file="$1"
  line="export PATH=\"${BIN_DIR}:\$PATH\""
  if [ -f "$file" ]; then
    if grep -qs "$line" "$file"; then
      return 0
    fi
  fi
  printf '\n%s\n' "$line" >> "$file"
}

if case ":$PATH:" in *":${BIN_DIR}:"*) true ;; *) false ;; esac; then
  :
else
  if [ -n "${FERRET_ADD_TO_PATH-}" ] && [ "${FERRET_ADD_TO_PATH}" = "0" ]; then
    echo "Add to PATH:"
    echo "  export PATH=\"${BIN_DIR}:\$PATH\""
  else
    shell_name="$(basename "${SHELL:-}")"
    if [ "$shell_name" = "zsh" ]; then
      add_path_line "${HOME}/.zshrc"
    elif [ "$shell_name" = "bash" ]; then
      add_path_line "${HOME}/.bashrc"
    fi
    add_path_line "${HOME}/.profile"
    echo "Added ${BIN_DIR} to PATH in your shell profile."
    echo "Restart your terminal to use 'ferret'."
  fi
fi
