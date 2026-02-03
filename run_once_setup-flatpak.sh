#!/bin/bash
# Setup Flatpak and Flathub repository

set -euo pipefail

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[*]${NC} $1"; }
warn() { echo -e "${YELLOW}[!]${NC} $1"; }

# Check if flatpak is installed
if ! command -v flatpak &>/dev/null; then
    warn "Flatpak not installed, skipping"
    exit 0
fi

log "Setting up Flatpak..."

# Add user to flatpak group if it exists
if getent group flatpak &>/dev/null; then
    sudo usermod -aG flatpak "$USER"
    log "Added $USER to flatpak group"
fi

# Add Flathub repository
flatpak remote-add --if-not-exists --user flathub https://flathub.org/repo/flathub.flatpakrepo

log "Flathub repository added"
