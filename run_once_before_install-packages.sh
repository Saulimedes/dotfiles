#!/bin/bash

# Core system package installation for Arch Linux
# This script replaces the Ansible-based package management with a chezmoi-native approach

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

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

    log_info "Installing packages on Arch Linux using pacman"
    sudo pacman -S --needed --noconfirm "${packages[@]}" || {
        log_error "Failed to install some packages"
        return 1
    }
}

# Core packages
core_packages=(
    dosfstools
    git
    git-lfs
    git-crypt
    chezmoi
    nmon
    bottom
    socat
    emacs
    bat
    bat-extras
    direnv
    ansible
    ripgrep
    gzip
    gettext
    pyenv
    p7zip
    fastfetch
    fzf
    fd
    ansible-lint
    tealdeer
    eza
    strace
    yazi
    sshpass
    gopass
    atuin
    zip
    unrar
    btop
    shellcheck
    pwgen
    gcc
    make
    openssl
    tcpdump
    nmap
    zoxide
    jq
    yq
    inetutils
    net-tools
    bind
    starship
    openbsd-netcat
    hyperfine
    nodejs
    curl
    pandoc
    tmux
    kitty
)

log_info "Starting package installation for Arch Linux"

# Install core packages
install_packages "${core_packages[@]}"

# Set up flatpak
if ! command -v flatpak &> /dev/null; then
    log_info "Installing flatpak"
    sudo pacman -S --needed --noconfirm flatpak
fi

# Add Flathub repository
if command -v flatpak &> /dev/null; then
    log_info "Adding Flathub repository"
    sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo || log_warn "Flathub might already be added"
fi

# Install paru AUR helper if not present
if ! command -v paru &> /dev/null && ! command -v yay &> /dev/null; then
    log_info "Installing paru AUR helper"

    # Install rust if not already installed (needed for paru)
    if ! command -v rustc &> /dev/null; then
        sudo pacman -S --needed --noconfirm rust
    fi

    # Clone and build paru
    cd /tmp
    git clone https://aur.archlinux.org/paru.git
    cd paru
    makepkg -si --noconfirm
    cd "$HOME"
    rm -rf /tmp/paru

    log_info "paru AUR helper installed successfully"
else
    log_info "AUR helper already available"
fi

# Change shell to zsh if available
if command -v zsh &> /dev/null; then
    current_shell=$(getent passwd "$USER" | cut -d: -f7)
    zsh_path=$(command -v zsh)

    if [[ "$current_shell" != "$zsh_path" ]]; then
        log_info "Changing shell to zsh"
        sudo chsh -s "$zsh_path" "$USER" || log_warn "Failed to change shell to zsh"
    else
        log_info "Shell is already zsh"
    fi
else
    log_warn "zsh not found, keeping current shell"
fi

log_info "Core package installation completed"
