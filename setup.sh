#!/usr/bin/env bash
#
# Gentoo Setup Script
# Usage: ./setup.sh [command...]
#
# Commands:
#   portage    - Configure portage, overlays (guru, xlibre)
#   base       - Core CLI tools
#   desktop    - XFCE, fonts, audio, media
#   dev        - Development tools and languages
#   work       - Kubernetes and cloud tools
#   browsers   - Librewolf, Brave, Helium
#   virt       - Podman, libvirt/QEMU
#   services   - Systemd user services
#   kernel     - Kernel dependencies (no build)
#   all        - Everything
#
set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log()     { echo -e "${GREEN}[*]${NC} $1"; }
warn()    { echo -e "${YELLOW}[!]${NC} $1"; }
error()   { echo -e "${RED}[x]${NC} $1"; }
section() { echo -e "\n${BLUE}=== $1 ===${NC}"; }

# Check if running on Gentoo
check_gentoo() {
    if [[ ! -f /etc/gentoo-release ]]; then
        error "This script is for Gentoo Linux only"
        exit 1
    fi
}

# Install packages (idempotent)
emerge_install() {
    sudo emerge --noreplace --quiet-build "$@"
}

#
# PORTAGE - Overlays and configuration
#
setup_portage() {
    section "Portage Configuration"

    log "Installing eselect-repository..."
    emerge_install app-eselect/eselect-repository

    log "Enabling guru overlay..."
    sudo eselect repository enable guru 2>/dev/null || true

    log "Syncing repositories..."
    sudo emaint sync -a

    log "Configuring package.accept_keywords..."
    sudo mkdir -p /etc/portage/package.accept_keywords
    sudo tee /etc/portage/package.accept_keywords/custom > /dev/null << 'EOF'
# Accept unstable for modern CLI tools
app-shells/atuin ~amd64
app-shells/starship ~amd64
app-shells/zoxide ~amd64
sys-apps/eza ~amd64
app-misc/fastfetch ~amd64

# Browsers
dev-util/github-cli ~amd64
media-fonts/nerdfonts ~amd64
www-client/brave-bin ~amd64
www-client/helium-bin ~amd64

# Kubernetes tools
dev-util/k9s ~amd64
EOF

    log "Configuring package.use..."
    sudo mkdir -p /etc/portage/package.use
    sudo tee /etc/portage/package.use/custom > /dev/null << 'EOF'
# Required for podman
net-firewall/iptables nftables
EOF

    log "Portage configured"
}

#
# BASE - Core CLI tools
#
install_base() {
    section "Base Packages"

    local packages=(
        # Archives
        app-arch/zip
        app-arch/unzip
        app-arch/p7zip
        app-arch/unrar

        # Version control
        dev-vcs/git
        dev-vcs/git-lfs

        # Shells
        app-shells/zsh
        app-shells/zsh-completions
        app-shells/bash-completion

        # Terminals
        x11-terms/kitty
        app-misc/tmux

        # Editors
        app-editors/vim
        app-editors/neovim
        app-editors/emacs

        # Modern CLI
        sys-apps/bat
        sys-apps/eza
        sys-apps/fd
        sys-apps/ripgrep
        app-shells/zoxide
        app-shells/fzf
        app-shells/direnv
        app-shells/starship
        app-shells/atuin

        # Monitoring
        sys-process/btop
        sys-process/htop
        app-misc/fastfetch
        sys-fs/ncdu
        sys-fs/duf

        # Network
        net-misc/curl
        net-misc/wget
        net-analyzer/nmap
        net-dns/bind-tools

        # JSON/YAML
        app-misc/jq

        # System
        sys-apps/tree
        sys-apps/pciutils
        sys-apps/usbutils
        sys-apps/lm-sensors
        sys-devel/make
        sys-devel/gcc

        # Docs
        app-misc/tealdeer
    )

    log "Installing ${#packages[@]} packages..."
    emerge_install "${packages[@]}" || warn "Some packages may have failed"

    # Set zsh as default shell
    if [[ "$SHELL" != */zsh ]]; then
        log "Setting zsh as default shell..."
        chsh -s /bin/zsh
    fi

    # Flatpak
    log "Setting up Flatpak..."
    emerge_install sys-apps/flatpak
    sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

    log "Base packages installed"
}

#
# DESKTOP - XFCE, fonts, audio, media
#
install_desktop() {
    section "Desktop Environment"

    local packages=(
        # Xlibre X server
        x11-base/xlibre-server
        x11-base/xlibre-drivers

        # XFCE
        xfce-base/xfce4-meta
        x11-misc/lightdm
        x11-misc/lightdm-gtk-greeter
        xfce-extra/thunar-archive-plugin
        xfce-base/thunar-volman
        xfce-extra/xfce4-notifyd
        xfce-extra/xfce4-screenshooter
        xfce-extra/xfce4-pulseaudio-plugin

        # GTK/Wayland
        x11-libs/gtk+:3
        dev-libs/wayland
        sys-apps/xdg-desktop-portal
        sys-apps/xdg-desktop-portal-gtk

        # Audio
        media-video/pipewire
        media-video/wireplumber
        media-sound/pavucontrol

        # Bluetooth
        net-wireless/bluez
        net-wireless/blueman

        # Media
        media-video/mpv
        media-video/vlc
        media-video/ffmpeg
        net-misc/yt-dlp
        media-gfx/imagemagick
        media-gfx/feh

        # Network
        net-misc/networkmanager
        gnome-extra/nm-applet
        net-vpn/openvpn

        # Utilities
        x11-misc/xdg-user-dirs
        x11-misc/xdg-utils
        x11-misc/xclip
        gui-apps/wl-clipboard

        # Sync
        net-p2p/syncthing

        # Printing
        net-print/cups

        # Theming
        x11-themes/arc-theme
        x11-themes/papirus-icon-theme
    )

    log "Installing desktop packages..."
    emerge_install "${packages[@]}" || warn "Some packages may have failed"

    # Fonts
    local fonts=(
        media-fonts/nerdfonts
        media-fonts/noto
        media-fonts/noto-emoji
        media-fonts/roboto
        media-fonts/liberation-fonts
        media-fonts/fira-code
        media-fonts/jetbrains-mono
        media-fonts/fontawesome
    )

    log "Installing fonts..."
    emerge_install "${fonts[@]}" || warn "Some fonts may have failed"

    # Services
    log "Enabling services..."
    sudo rc-update add display-manager default 2>/dev/null || true
    sudo rc-update add NetworkManager default 2>/dev/null || true
    sudo rc-update add bluetooth default 2>/dev/null || true
    sudo rc-update add cupsd default 2>/dev/null || true

    # LightDM config
    echo 'DISPLAYMANAGER="lightdm"' | sudo tee /etc/conf.d/display-manager > /dev/null

    # User groups
    sudo usermod -aG video,audio,input,plugdev,lp "$USER"

    # Flatpak apps
    log "Installing Flatpak apps..."
    flatpak install -y flathub org.telegram.desktop 2>/dev/null || true
    flatpak install -y flathub us.zoom.Zoom 2>/dev/null || true

    # Create directories
    mkdir -p ~/Projects ~/Documents/org
    xdg-user-dirs-update 2>/dev/null || true

    log "Desktop installed"
}

#
# DEV - Development tools
#
install_dev() {
    section "Development Tools"

    local packages=(
        # Languages
        net-libs/nodejs
        dev-lang/python
        dev-python/pip
        dev-lang/go
        dev-lang/rust
        dev-lang/deno-bin

        # Build
        dev-build/cmake
        dev-build/ninja
        dev-build/meson

        # Databases
        dev-db/postgresql
        dev-db/sqlite

        # IaC
        app-admin/terraform

        # Tools
        dev-util/github-cli
        dev-util/shellcheck
        dev-util/shfmt
    )

    log "Installing dev packages..."
    emerge_install "${packages[@]}" || warn "Some packages may have failed"

    # Bun
    if [[ ! -f ~/.bun/bin/bun ]]; then
        log "Installing bun..."
        curl -fsSL https://bun.sh/install | bash || warn "Bun install failed"
    fi

    # Rustup
    if [[ ! -f ~/.cargo/bin/rustup ]]; then
        log "Installing rustup..."
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y || warn "Rustup install failed"
    fi

    log "Development tools installed"
}

#
# WORK - Kubernetes and cloud tools
#
install_work() {
    section "Work Tools (Kubernetes/Cloud)"

    local packages=(
        sys-cluster/kubectl
        app-admin/helm
        dev-util/kustomize
        app-admin/vault
    )

    log "Installing k8s packages..."
    emerge_install "${packages[@]}" || warn "Some packages may have failed"

    # k9s
    if [[ ! -f /usr/local/bin/k9s ]]; then
        log "Installing k9s..."
        curl -sL https://github.com/derailed/k9s/releases/latest/download/k9s_Linux_amd64.tar.gz \
            | sudo tar xz -C /usr/local/bin k9s || warn "k9s install failed"
    fi

    # kubie
    if [[ ! -f /usr/local/bin/kubie ]]; then
        log "Installing kubie..."
        sudo curl -sL -o /usr/local/bin/kubie \
            https://github.com/sbstp/kubie/releases/latest/download/kubie-linux-amd64
        sudo chmod +x /usr/local/bin/kubie
    fi

    # stern
    if [[ ! -f /usr/local/bin/stern ]]; then
        log "Installing stern..."
        curl -sL https://github.com/stern/stern/releases/latest/download/stern_linux_amd64.tar.gz \
            | sudo tar xz -C /usr/local/bin stern || warn "stern install failed"
    fi

    log "Work tools installed"
}

#
# BROWSERS - Librewolf, Brave, Helium
#
install_browsers() {
    section "Browsers"

    local packages=(
        www-client/firefox-bin
        www-client/brave-bin
        www-client/helium-bin
    )

    log "Installing browsers..."
    emerge_install "${packages[@]}" || warn "Some browsers may have failed"

    # Set default browser
    xdg-settings set default-web-browser librewolf.desktop 2>/dev/null || true

    log "Browsers installed"
}

#
# VIRT - Containers and VMs
#
install_virt() {
    section "Virtualisation"

    local packages=(
        # Podman
        app-containers/podman
        app-containers/buildah
        app-containers/skopeo
        app-containers/crun
        app-containers/slirp4netns
        sys-fs/fuse-overlayfs
        app-containers/netavark
        app-containers/aardvark-dns

    )

    log "Installing virt packages..."
    emerge_install "${packages[@]}" || warn "Some packages may have failed"

    # Groups for podman
    sudo usermod -aG kvm "$USER" 2>/dev/null || true

    # Rootless podman
    grep -q "^$USER:" /etc/subuid || echo "$USER:100000:65536" | sudo tee -a /etc/subuid
    grep -q "^$USER:" /etc/subgid || echo "$USER:100000:65536" | sudo tee -a /etc/subgid

    log "Virtualisation installed"
}

#
# SERVICES - Systemd user services
#
setup_services() {
    section "User Services"

    mkdir -p ~/.config/systemd/user

    # Syncthing
    cat > ~/.config/systemd/user/syncthing.service << 'EOF'
[Unit]
Description=Syncthing
After=network.target

[Service]
ExecStart=/usr/bin/syncthing serve --no-browser --no-restart --logflags=0
Restart=on-failure

[Install]
WantedBy=default.target
EOF

    # Podman cleanup
    cat > ~/.config/systemd/user/podman-cleanup.service << 'EOF'
[Unit]
Description=Podman cleanup

[Service]
Type=oneshot
ExecStart=/usr/bin/podman system prune -af
EOF

    cat > ~/.config/systemd/user/podman-cleanup.timer << 'EOF'
[Unit]
Description=Weekly podman cleanup

[Timer]
OnCalendar=weekly
Persistent=true

[Install]
WantedBy=timers.target
EOF

    systemctl --user daemon-reload
    systemctl --user enable --now syncthing.service 2>/dev/null || true
    systemctl --user enable --now podman-cleanup.timer 2>/dev/null || true

    log "Services configured"
}

#
# KERNEL - Dependencies only
#
install_kernel() {
    section "Kernel Dependencies"

    local packages=(
        sys-kernel/linux-firmware
        sys-kernel/gentoo-sources
        sys-kernel/genkernel
        sys-boot/grub
    )

    log "Installing kernel dependencies..."
    emerge_install "${packages[@]}"

    log "Kernel deps installed (run genkernel manually to build)"
}

#
# HELP
#
show_help() {
    cat << 'EOF'
Gentoo Setup Script

Usage: ./setup.sh [command...]

Commands:
  portage    Configure portage and overlays (guru, xlibre)
  base       Core CLI tools (zsh, tmux, bat, eza, etc.)
  desktop    XFCE, Xlibre, fonts, audio, media
  dev        Development tools (nodejs, python, go, rust)
  work       Kubernetes tools (kubectl, helm, k9s)
  browsers   Librewolf, Brave, Helium
  virt       Podman, libvirt/QEMU
  services   Systemd user services
  kernel     Kernel build dependencies
  all        Install everything

Examples:
  ./setup.sh all              # Full install
  ./setup.sh portage base     # Minimal setup
  ./setup.sh desktop browsers # Desktop only
EOF
}

#
# MAIN
#
main() {
    check_gentoo

    if [[ $# -eq 0 ]]; then
        show_help
        exit 0
    fi

    for cmd in "$@"; do
        case "$cmd" in
            portage)  setup_portage ;;
            base)     install_base ;;
            desktop)  install_desktop ;;
            dev)      install_dev ;;
            work)     install_work ;;
            browsers) install_browsers ;;
            virt)     install_virt ;;
            services) setup_services ;;
            kernel)   install_kernel ;;
            all)
                setup_portage
                install_base
                install_desktop
                install_dev
                install_browsers
                install_virt
                setup_services
                ;;
            help|-h|--help)
                show_help
                ;;
            *)
                error "Unknown command: $cmd"
                show_help
                exit 1
                ;;
        esac
    done

    section "Done"
    log "Reboot recommended to apply all changes"
    log "Then run: chezmoi apply"
}

main "$@"
