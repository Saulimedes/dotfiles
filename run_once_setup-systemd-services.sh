#!/bin/bash

# Enable user systemd services for Arch Linux
# This replaces the systemd service management from the desktop Ansible role

set -euo pipefail

# (removed hostname check since only using Arch Linux)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if systemd is available
if ! command -v systemctl &> /dev/null; then
    log_error "systemctl not found, skipping systemd service setup"
    exit 0
fi

# Enable user systemd services from chezmoi-managed config
service_dir="$HOME/.config/systemd/user"

if [[ ! -d "$service_dir" ]]; then
    log_warn "No user systemd service directory found at $service_dir"
    exit 0
fi

log_info "Enabling user systemd services"

# Reload systemd user daemon
systemctl --user daemon-reload

# Find and enable all service files
for service_file in "$service_dir"/*.service; do
    if [[ -f "$service_file" ]]; then
        service_name=$(basename "$service_file")
        log_info "Enabling $service_name"
        
        if systemctl --user enable "$service_name"; then
            log_info "Successfully enabled $service_name"
        else
            log_warn "Failed to enable $service_name"
        fi
    fi
done

# Find and enable all timer files
for timer_file in "$service_dir"/*.timer; do
    if [[ -f "$timer_file" ]]; then
        timer_name=$(basename "$timer_file")
        log_info "Enabling and starting $timer_name"
        
        if systemctl --user enable --now "$timer_name"; then
            log_info "Successfully enabled and started $timer_name"
        else
            log_warn "Failed to enable $timer_name"
        fi
    fi
done

log_info "User systemd service setup completed"