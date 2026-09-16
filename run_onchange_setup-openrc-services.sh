#!/bin/bash
# OpenRC service management - reruns when this file changes
set -euo pipefail

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[*]${NC} $1"; }
warn() { echo -e "${YELLOW}[!]${NC} $1"; }

if [[ ! -f /etc/gentoo-release ]]; then
    warn "Not Gentoo, skipping OpenRC service setup"
    exit 0
fi

enable_service() {
    local svc="$1"
    local runlevel="${2:-default}"
    if [[ -f "/etc/init.d/$svc" ]]; then
        sudo rc-update add "$svc" "$runlevel" 2>/dev/null && log "Enabled $svc @ $runlevel" || true
    else
        warn "$svc not found in /etc/init.d, skipping"
    fi
}

start_service() {
    local svc="$1"
    if [[ -f "/etc/init.d/$svc" ]]; then
        sudo rc-service "$svc" start 2>/dev/null && log "Started $svc" || true
    fi
}

log "Configuring OpenRC services..."

# Time sync
enable_service chronyd

# Power management
enable_service tlp

# Bluetooth
enable_service bluetooth

# VPN / network extras
enable_service mullvad-daemon
enable_service i2pd

# Network filesystems
enable_service autofs

# Sync
enable_service syncthing

# Start chronyd now if not running
if [[ -f /etc/init.d/chronyd ]]; then
    if ! rc-service chronyd status &>/dev/null; then
        start_service chronyd
    fi
fi

log "OpenRC service setup complete"
