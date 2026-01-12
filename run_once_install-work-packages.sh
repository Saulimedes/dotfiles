#!/bin/bash

# Work-specific package installation for Arch Linux
# These are work-related tools that can be useful on any machine

set -euo pipefail

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

install_packages() {
    local packages=("$@")

    sudo pacman -S --needed --noconfirm "${packages[@]}" || {
        log_error "Failed to install some packages"
        return 1
    }
}

# Work-specific packages
work_packages=(
    kubectl
    kubie
    helm
    opentofu
    vault
)

log_info "Installing work-specific packages"
install_packages "${work_packages[@]}"

# Install AWS CLI (AUR)
if ! command -v aws &> /dev/null; then
    log_info "Installing AWS CLI"

    if command -v paru &> /dev/null; then
        paru -S --noconfirm aws-cli-v2
    elif command -v yay &> /dev/null; then
        yay -S --noconfirm aws-cli-v2
    else
        log_warn "No AUR helper found, skipping AWS CLI"
    fi
else
    log_info "AWS CLI already installed"
fi

# Install Azure CLI (AUR)
if ! command -v az &> /dev/null; then
    log_info "Installing Azure CLI"

    if command -v paru &> /dev/null; then
        paru -S --noconfirm azure-cli
    elif command -v yay &> /dev/null; then
        yay -S --noconfirm azure-cli
    else
        log_warn "No AUR helper found, skipping Azure CLI"
    fi
else
    log_info "Azure CLI already installed"
fi

# Install Google Cloud SDK (AUR)
if ! command -v gcloud &> /dev/null; then
    log_info "Installing Google Cloud SDK"

    if command -v paru &> /dev/null; then
        paru -S --noconfirm google-cloud-cli
    elif command -v yay &> /dev/null; then
        yay -S --noconfirm google-cloud-cli
    else
        log_warn "No AUR helper found, skipping Google Cloud SDK"
    fi
else
    log_info "Google Cloud SDK already installed"
fi

# Install Microsoft Edge browser (AUR)
if ! command -v microsoft-edge-stable &> /dev/null; then
    log_info "Installing Microsoft Edge browser"

    if command -v paru &> /dev/null; then
        paru -S --noconfirm microsoft-edge-stable-bin
    elif command -v yay &> /dev/null; then
        yay -S --noconfirm microsoft-edge-stable-bin
    else
        log_warn "No AUR helper found, skipping Microsoft Edge"
    fi
else
    log_info "Microsoft Edge already installed"
fi

log_info "Work package installation completed"
