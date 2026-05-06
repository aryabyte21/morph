#!/usr/bin/env sh
set -eu

REPO="${MORPH_REPO:-aryabyte21/morph}"
VERSION="${MORPH_VERSION:-latest}"
INSTALL_DIR="${MORPH_INSTALL_DIR:-/usr/local/bin}"

uname_s=$(uname -s)
uname_m=$(uname -m)

case "$uname_s" in
  Darwin)  os=darwin ;;
  Linux)   os=linux ;;
  *)       echo "unsupported OS: $uname_s" >&2; exit 1 ;;
esac

case "$uname_m" in
  arm64|aarch64) arch=arm64 ;;
  x86_64|amd64)  arch=amd64 ;;
  *)             echo "unsupported arch: $uname_m" >&2; exit 1 ;;
esac

if [ "$VERSION" = "latest" ]; then
  url="https://github.com/${REPO}/releases/latest/download/morph-${os}-${arch}.tar.gz"
else
  url="https://github.com/${REPO}/releases/download/${VERSION}/morph-${os}-${arch}.tar.gz"
fi

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

echo "morph: downloading $url"
curl -fsSL "$url" -o "$tmpdir/morph.tar.gz"

tar -xzf "$tmpdir/morph.tar.gz" -C "$tmpdir"

if [ ! -x "$tmpdir/morph" ]; then
  echo "morph: extracted archive missing 'morph' executable" >&2
  exit 1
fi

if [ -w "$INSTALL_DIR" ]; then
  install -m 0755 "$tmpdir/morph" "$INSTALL_DIR/morph"
else
  echo "morph: $INSTALL_DIR not writable; using sudo"
  sudo install -m 0755 "$tmpdir/morph" "$INSTALL_DIR/morph"
fi

echo "morph: installed to $INSTALL_DIR/morph"
"$INSTALL_DIR/morph" --version
