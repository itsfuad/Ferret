#!/usr/bin/env sh

set -eu

# Detect if running on Arch Linux
if [ ! -f /etc/arch-release ]; then
  echo "This installer is intended for Arch Linux." >&2
  exit 1
fi

REPO="${FERRET_REPO:-https://github.com/Ferret-Language/Ferret.git}"
REF="${FERRET_REF:-stable}"
SRC_DIR="${FERRET_SRC_DIR:-${HOME}/.local/src/Ferret}"
DEST_DIR="${FERRET_INSTALL_DIR:-${HOME}/.local}"

# Check if user wants system-wide install
if [ "${FERRET_INSTALL_DIR:-}" = "" ] && [ "$(id -u)" -eq 0 ]; then
  DEST_DIR="/usr/local"
fi

echo "Installing Ferret to ${DEST_DIR}"

# Install dependencies via pacman (requires sudo if not root)
if [ "$(id -u)" -eq 0 ]; then
  pacman -S --needed --noconfirm base-devel git go clang binutils
else
  echo "Note: This script may need sudo privileges to install packages."
  echo "Installing dependencies..."
  if ! sudo pacman -S --needed --noconfirm base-devel git go clang binutils 2>/dev/null; then
    echo "Warning: Could not install packages. Please install manually:" >&2
    echo "  sudo pacman -S base-devel git go clang binutils" >&2
    echo "Continuing anyway..." >&2
  fi
fi

# Clone or update repository
if [ -d "${SRC_DIR}/.git" ]; then
  git -C "${SRC_DIR}" fetch --tags origin
  git -C "${SRC_DIR}" checkout "${REF}"
  git -C "${SRC_DIR}" pull --ff-only
else
  mkdir -p "$(dirname "${SRC_DIR}")"
  git clone --depth 1 --branch "${REF}" "${REPO}" "${SRC_DIR}"
fi

cd "${SRC_DIR}"

# Set up environment
export CC=clang
export FERRET_CC=clang
export CGO_ENABLED=1
export FERRET_BUNDLE_TOOLCHAIN=1

LIBS_DIR="${SRC_DIR}/libs"
RUNTIME_DIR="${SRC_DIR}/runtime"
FERRET_LIBS_DIR="${SRC_DIR}/ferret_libs"

# Clean and prepare libs directory
rm -rf "${LIBS_DIR}"
mkdir -p "${LIBS_DIR}"

# Validate directories
if [ ! -d "${FERRET_LIBS_DIR}" ]; then
  echo "Missing ferret_libs directory in ${SRC_DIR}" >&2
  exit 1
fi

if [ ! -d "${RUNTIME_DIR}" ]; then
  echo "Missing runtime directory in ${SRC_DIR}" >&2
  exit 1
fi

# Copy ferret libraries
find "${FERRET_LIBS_DIR}" -type f -name '*.fer' | while IFS= read -r file; do
  rel="${file#${FERRET_LIBS_DIR}/}"
  dest="${LIBS_DIR}/${rel}"
  mkdir -p "$(dirname "${dest}")"
  cp -f "${file}" "${dest}"
done

# Build runtime library
OBJ_DIR="$(mktemp -d)"
trap "rm -rf ${OBJ_DIR}" EXIT

found_c=0
for src in "${RUNTIME_DIR}"/core/*.c "${RUNTIME_DIR}"/libs/*.c; do
  if [ -f "${src}" ]; then
    found_c=1
    base="$(basename "${src}" .c)"
    obj="${OBJ_DIR}/${base}.o"
    clang -std=c99 -O2 -w -fno-pie -I "${RUNTIME_DIR}/core" -I "${RUNTIME_DIR}/libs" -c "${src}" -o "${obj}"
  fi
done

if [ "${found_c}" -ne 1 ]; then
  echo "No runtime C files found in ${RUNTIME_DIR}/core or ${RUNTIME_DIR}/libs" >&2
  exit 1
fi

ar rcs "${LIBS_DIR}/libferret_runtime.a" "${OBJ_DIR}"/*.o

if command -v ranlib >/dev/null 2>&1; then
  ranlib "${LIBS_DIR}/libferret_runtime.a"
fi

rm -rf "${OBJ_DIR}"
trap - EXIT

# Run bootstrap to set up toolchain
if [ -f "${SRC_DIR}/tools/main.go" ]; then
  echo "Running bootstrap..."
  go run "${SRC_DIR}/tools/main.go" || {
    echo "Warning: Bootstrap failed. Continuing anyway..." >&2
  }
fi

# Build the compiler
echo "Building Ferret compiler..."
go build -o bin/ferret

# Create destination directories
mkdir -p "${DEST_DIR}/bin" "${DEST_DIR}/libs"

# Install binary
cp -f bin/ferret "${DEST_DIR}/bin/ferret-bin"

# Install libraries
rm -rf "${DEST_DIR}/libs"
cp -R libs "${DEST_DIR}/"

# Create wrapper script
wrapper="${DEST_DIR}/bin/ferret"
cat > "${wrapper}" << 'WRAPPER_EOF'
#!/usr/bin/env sh
set -e

# Use the bundled toolchain if available
if [ -d "$(dirname "$0")/../libs/toolchain" ]; then
  export FERRET_TOOLCHAIN_PATH="$(dirname "$0")/../libs/toolchain"
fi

exec "$(dirname "$0")/ferret-bin" "$@"
WRAPPER_EOF

chmod 755 "${wrapper}"

echo ""
echo "✓ Ferret installed successfully to ${DEST_DIR}"
echo ""
echo "Add to your PATH if needed:"
echo "  export PATH=\"${DEST_DIR}/bin:\$PATH\""
echo ""
echo "Or add to your shell config (e.g., ~/.bashrc or ~/.zshrc):"
echo "  export PATH=\"${DEST_DIR}/bin:\$PATH\""
