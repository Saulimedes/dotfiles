#!/bin/bash
# Install packages from all packages*.txt files
# Runs once via chezmoi, or manually: ./run_once_install-packages.sh

set -euo pipefail

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[*]${NC} $1"; }
warn() { echo -e "${YELLOW}[!]${NC} $1"; }

# Only run on Gentoo
if [[ ! -f /etc/gentoo-release ]]; then
    warn "Not Gentoo, skipping"
    exit 0
fi

PACKAGES_DIR="${HOME}/.local/share/chezmoi"

# Read packages from all package lists (skip comments and empty lines)
read_packages() {
    cat "$PACKAGES_DIR"/packages*.txt 2>/dev/null | grep -v '^#' | grep -v '^$' | sort -u | tr '\n' ' '
}

install_packages() {
    local packages
    packages=$(read_packages)

    if [[ -z "$packages" ]]; then
        warn "No packages found in $PACKAGES_DIR/packages*.txt"
        return
    fi

    log "Installing packages from $PACKAGES_DIR/packages*.txt..."
    sudo emerge --noreplace --quiet-build $packages || warn "Some packages may have failed"
}

# Install clipboard tool based on display server
install_clipboard() {
    if [[ "$XDG_SESSION_TYPE" == "wayland" ]] || [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
        log "Wayland detected, installing wl-clipboard..."
        sudo emerge --noreplace --quiet-build gui-apps/wl-clipboard || warn "wl-clipboard install failed"
    elif [[ "$XDG_SESSION_TYPE" == "x11" ]] || [[ -n "${DISPLAY:-}" ]]; then
        log "X11 detected, installing xclip..."
        sudo emerge --noreplace --quiet-build x11-misc/xclip || warn "xclip install failed"
    else
        warn "No display server detected, skipping clipboard tool"
    fi
}

main() {
    log "Package installation"
    install_packages
    install_clipboard
    log "Done"
}

main
