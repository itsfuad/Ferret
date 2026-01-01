#!/usr/bin/env sh
set -eu

if [ -z "${TERMUX_VERSION-}" ] && [ "${PREFIX:-}" != "/data/data/com.termux/files/usr" ]; then
  echo "This installer is intended for Termux." >&2
  exit 1
fi

REPO="${FERRET_REPO:-https://github.com/Ferret-Language/Ferret.git}"
REF="${FERRET_REF:-stable}"
SRC_DIR="${FERRET_SRC_DIR:-${HOME}/Ferret}"
DEST_DIR="${FERRET_INSTALL_DIR:-${PREFIX:-/data/data/com.termux/files/usr}}"

pkg update -y
pkg upgrade -y
pkg install -y git golang clang binutils libxml2

if [ -d "${SRC_DIR}/.git" ]; then
  git -C "${SRC_DIR}" fetch --tags origin
  git -C "${SRC_DIR}" checkout "${REF}"
  git -C "${SRC_DIR}" pull --ff-only
else
  git clone --depth 1 --branch "${REF}" "${REPO}" "${SRC_DIR}"
fi

cd "${SRC_DIR}"

export CC=clang
export FERRET_CC=clang
export CGO_ENABLED=1
export FERRET_BUNDLE_TOOLCHAIN=0

LIBS_DIR="${SRC_DIR}/libs"
RUNTIME_DIR="${SRC_DIR}/runtime"
FERRET_LIBS_DIR="${SRC_DIR}/ferret_libs"

rm -rf "${LIBS_DIR}"
mkdir -p "${LIBS_DIR}"

if [ ! -d "${FERRET_LIBS_DIR}" ]; then
  echo "Missing ferret_libs directory in ${SRC_DIR}" >&2
  exit 1
fi

if [ ! -d "${RUNTIME_DIR}" ]; then
  echo "Missing runtime directory in ${SRC_DIR}" >&2
  exit 1
fi

find "${FERRET_LIBS_DIR}" -type f -name '*.fer' | while IFS= read -r file; do
  rel="${file#${FERRET_LIBS_DIR}/}"
  dest="${LIBS_DIR}/${rel}"
  mkdir -p "$(dirname "${dest}")"
  cp -f "${file}" "${dest}"
done

OBJ_DIR="$(mktemp -d)"
found_c=0
for src in "${RUNTIME_DIR}"/core/*.c "${RUNTIME_DIR}"/libs/*.c; do
  if [ -f "${src}" ]; then
    found_c=1
    base="$(basename "${src}" .c)"
    obj="${OBJ_DIR}/${base}.o"
    clang -std=c99 -O2 -w -fPIC -I "${RUNTIME_DIR}/core" -I "${RUNTIME_DIR}/libs" -c "${src}" -o "${obj}"
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

go build -o bin/ferret

mkdir -p "${DEST_DIR}/bin" "${DEST_DIR}/libs"
cp -f bin/ferret "${DEST_DIR}/bin/ferret-bin"
rm -rf "${DEST_DIR}/libs"
cp -R libs "${DEST_DIR}/"

wrapper="${DEST_DIR}/bin/ferret"
cat > "${wrapper}" <<'EOF'
#!/data/data/com.termux/files/usr/bin/sh
set -e

if ! command -v clang >/dev/null 2>&1; then
  echo "Ferret: clang is required on Termux." >&2
  exit 1
fi

export FERRET_LD=clang
EOF
cat >> "${wrapper}" <<'EOF'
exec "$PREFIX/bin/ferret-bin" "$@"
EOF
chmod 755 "${wrapper}"

echo "Installed to ${DEST_DIR}"
echo ""
# Check if installed to default Termux location (which is in PATH by default)
default_prefix="/data/data/com.termux/files/usr"
if [ "${DEST_DIR}" = "${PREFIX:-${default_prefix}}" ] || [ "${DEST_DIR}" = "${default_prefix}" ]; then
  echo "✓ The 'ferret' command should be available now (${DEST_DIR}/bin is in PATH by default in Termux)"
  echo "  If 'ferret' doesn't work, try: exec \$SHELL"
else
  echo "To use the 'ferret' command, add to your PATH:"
  echo "  export PATH=\"${DEST_DIR}/bin:\$PATH\""
  echo "Or add to ~/.bashrc:"
  echo "  echo 'export PATH=\"${DEST_DIR}/bin:\$PATH\"' >> ~/.bashrc"
fi
