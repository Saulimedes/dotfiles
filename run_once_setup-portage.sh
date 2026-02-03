#!/bin/bash
# Initial Gentoo portage setup - runs once per machine

set -euo pipefail

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[*]${NC} $1"; }
warn() { echo -e "${YELLOW}[!]${NC} $1"; }

# Only run on Gentoo
if [[ ! -f /etc/gentoo-release ]]; then
    warn "Not Gentoo, skipping portage setup"
    exit 0
fi

log "Setting up Gentoo portage..."

# Install eselect-repository if needed
if ! command -v eselect &>/dev/null || ! eselect repository list &>/dev/null 2>&1; then
    log "Installing eselect-repository..."
    sudo emerge --noreplace app-eselect/eselect-repository
fi

# Enable overlays
log "Enabling overlays..."
sudo eselect repository enable guru 2>/dev/null || true
sudo eselect repository enable librewolf 2>/dev/null || true

# Sync repositories
log "Syncing repositories..."
sudo emaint sync -a

log "Portage setup complete"
