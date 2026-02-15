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

# Set XDG_DATA_DIRS so flatpak doesn't warn during bootstrap
export XDG_DATA_DIRS="/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"

log "Setting up Flatpak..."

# Add user to flatpak group if it exists
if getent group flatpak &>/dev/null; then
    sudo usermod -aG flatpak "$USER"
    log "Added $USER to flatpak group"
fi

# Add Flathub repository
flatpak remote-add --if-not-exists --user flathub https://flathub.org/repo/flathub.flatpakrepo

log "Flathub repository added"

# Install Flatpak applications
log "Installing Flatpak applications..."

flatpak install --user --noninteractive flathub com.valvesoftware.Steam
log "Steam installed"

flatpak install --user --noninteractive flathub im.riot.Riot
log "Element installed"

flatpak install --user --noninteractive flathub io.github.victoralvesf.aonsoku
log "Aonsoku installed"

flatpak install --user --noninteractive flathub org.libreoffice.LibreOffice
log "LibreOffice installed"

flatpak install --user --noninteractive flathub org.jdownloader.JDownloader
log "JDownloader installed"
