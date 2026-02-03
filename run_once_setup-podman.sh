#!/bin/bash
# Setup rootless podman

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

# Check if podman is installed
if ! command -v podman &>/dev/null; then
    warn "Podman not installed, skipping"
    exit 0
fi

log "Setting up rootless podman..."

# Configure subuid/subgid for rootless containers
if ! grep -q "^$USER:" /etc/subuid 2>/dev/null; then
    log "Adding $USER to /etc/subuid"
    echo "$USER:100000:65536" | sudo tee -a /etc/subuid
fi

if ! grep -q "^$USER:" /etc/subgid 2>/dev/null; then
    log "Adding $USER to /etc/subgid"
    echo "$USER:100000:65536" | sudo tee -a /etc/subgid
fi

# Add user to required groups
sudo usermod -aG kvm "$USER" 2>/dev/null || true

log "Podman rootless setup complete"
log "You may need to log out and back in for group changes to take effect"
