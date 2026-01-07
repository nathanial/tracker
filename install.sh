#!/bin/bash
set -e

# Tracker install script
# Builds and installs the tracker binary

INSTALL_DIR="${INSTALL_DIR:-$HOME/.local/bin}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Building tracker..."
cd "$SCRIPT_DIR"
lake build tracker

echo "Installing to $INSTALL_DIR..."
mkdir -p "$INSTALL_DIR"
cp .lake/build/bin/tracker "$INSTALL_DIR/tracker"
chmod +x "$INSTALL_DIR/tracker"
# Remove quarantine/provenance attributes
xattr -cr "$INSTALL_DIR/tracker" 2>/dev/null || true
# Re-sign the binary (required on macOS after copying)
codesign -fs - "$INSTALL_DIR/tracker" 2>/dev/null || true

echo ""
echo "Installed tracker to $INSTALL_DIR/tracker"

# Check if install dir is in PATH
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    echo ""
    echo "NOTE: $INSTALL_DIR is not in your PATH."
    echo "Add it by adding this line to your shell config (~/.bashrc, ~/.zshrc, etc.):"
    echo ""
    echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
    echo ""
fi

echo "Run 'tracker --help' to get started."
