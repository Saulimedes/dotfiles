#!/bin/bash
# Install mpv plugins: mpv360, gif-generator, webm, jellyfin-mpv-shim

set -euo pipefail

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[*]${NC} $1"; }
warn() { echo -e "${YELLOW}[!]${NC} $1"; }

MPV_DIR="$HOME/.config/mpv"
SCRIPTS_DIR="$MPV_DIR/scripts"
SHADERS_DIR="$MPV_DIR/shaders"
SCRIPT_OPTS_DIR="$MPV_DIR/script-opts"

mkdir -p "$SCRIPTS_DIR" "$SHADERS_DIR" "$SCRIPT_OPTS_DIR"

# mpv360 - 360° video with mouse drag navigation
if [[ ! -d "$SCRIPTS_DIR/mpv360" ]]; then
    log "Installing mpv360..."
    TMPDIR=$(mktemp -d)
    git clone --depth 1 https://github.com/kasper93/mpv360.git "$TMPDIR/mpv360"
    cp -r "$TMPDIR/mpv360/scripts/"* "$SCRIPTS_DIR/"
    cp -r "$TMPDIR/mpv360/shaders/"* "$SHADERS_DIR/"
    # Copy default script-opts only if not already managed by chezmoi
    if [[ ! -f "$SCRIPT_OPTS_DIR/mpv360.conf" ]]; then
        cp -r "$TMPDIR/mpv360/script-opts/"* "$SCRIPT_OPTS_DIR/"
    fi
    rm -rf "$TMPDIR"
    log "mpv360 installed"
else
    log "mpv360 already installed, skipping"
fi

# mpv-gif-generator
if [[ ! -f "$SCRIPTS_DIR/mpv-gif.lua" ]]; then
    log "Installing mpv-gif-generator..."
    curl -fSL -o "$SCRIPTS_DIR/mpv-gif.lua" \
        "https://raw.githubusercontent.com/the-honey/mpv-gif-generator/master/mpv-gif.lua"
    log "gif-generator installed"
else
    log "gif-generator already installed, skipping"
fi

# mpv-webm
if [[ ! -f "$SCRIPTS_DIR/webm.lua" ]]; then
    log "Installing mpv-webm..."
    curl -fSL -o "$SCRIPTS_DIR/webm.lua" \
        "https://github.com/ekisu/mpv-webm/releases/download/latest/webm.lua"
    log "mpv-webm installed"
else
    log "mpv-webm already installed, skipping"
fi

# jellyfin-mpv-shim via flatpak
if command -v flatpak &>/dev/null; then
    if ! flatpak list --user | grep -q jellyfin-mpv-shim; then
        log "Installing jellyfin-mpv-shim via flatpak..."
        flatpak install --user -y flathub com.github.iwalton3.jellyfin-mpv-shim || \
            warn "Failed to install jellyfin-mpv-shim (flathub may not be set up)"
    else
        log "jellyfin-mpv-shim already installed, skipping"
    fi
else
    warn "Flatpak not available, skipping jellyfin-mpv-shim"
fi

log "mpv plugins setup complete"
