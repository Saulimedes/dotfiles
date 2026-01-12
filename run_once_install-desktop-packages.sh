#!/bin/bash

# Desktop environment and multimedia package installation for Arch Linux
# This replaces the desktop role from the Ansible configuration

set -euo pipefail

# Skip on headless systems or if DISPLAY is not set
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

install_packages() {
    local packages=("$@")

    sudo pacman -S --needed --noconfirm "${packages[@]}" || {
        log_error "Failed to install some packages"
        return 1
    }
}

# Font packages
font_packages=(
    ttf-font-awesome
    ttf-roboto
    ttf-droid
    ttf-liberation
    ttf-linux-libertine
    noto-fonts-emoji
)

# Desktop packages
desktop_packages=(
    docker
    docker-compose
    networkmanager
    networkmanager-vpnc
    networkmanager-openconnect
    networkmanager-openvpn
    nm-connection-editor
    mpv
    xdg-user-dirs
    xdg-utils
    ffmpeg
    gst-plugins-good
    gst-plugins-bad
    gst-plugins-ugly
    gst-libav
    vlc
    wl-clipboard
    rust
    android-tools
    yt-dlp
    syncthing
)

# Flatpak packages
flatpak_packages=(
    "org.jdownloader.JDownloader"
    "us.zoom.Zoom"
    "org.localsend.localsend_app"
    "org.telegram.desktop"
)

log_info "Installing font packages"
install_packages "${font_packages[@]}"

log_info "Installing desktop packages"
install_packages "${desktop_packages[@]}"

# Enable and start Docker service
log_info "Enabling Docker service"
sudo systemctl enable docker.service
sudo systemctl start docker.service

# Add user to docker group
log_info "Adding user to docker group"
sudo usermod -a -G docker "$USER"

# Install flatpak packages
if command -v flatpak &> /dev/null; then
    log_info "Installing flatpak packages"
    for package in "${flatpak_packages[@]}"; do
        if ! flatpak list --system | grep -q "$package"; then
            log_info "Installing $package"
            sudo flatpak install -y flathub "$package" || log_warn "Failed to install $package"
        else
            log_info "$package already installed"
        fi
    done
else
    log_warn "Flatpak not available, skipping flatpak packages"
fi

# Set up user directories
log_info "Setting up user directories"
xdg-user-dirs-update

# Create custom directories
mkdir -p "$HOME/Projects" "$HOME/Documents/org"

# Update XDG user directories config
mkdir -p "$HOME/.config"
cat >> "$HOME/.config/user-dirs.dirs" << 'EOF'
XDG_PROJECTS_DIR="$HOME/Projects"
XDG_ORG_DIR="$HOME/Documents/org"
EOF

# Add user to flatpak group (Arch-specific)
if getent group flatpak > /dev/null 2>&1; then
    sudo usermod -a -G flatpak "$USER"
    log_info "Added user to flatpak group"
fi

log_info "Desktop package installation completed"
log_warn "You may need to log out and log back in for docker group membership to take effect"
