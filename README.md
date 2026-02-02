![License: GPLv3](https://img.shields.io/badge/license-GPLv3-green.svg)
![Gentoo](https://img.shields.io/badge/distro-Gentoo-54487A?logo=gentoo&logoColor=white)
![Emacs](https://img.shields.io/badge/editor-emacs-7F5AB6?logo=gnuemacs&logoColor=white)
![Zsh](https://img.shields.io/badge/shell-zsh-green?logo=zsh&logoColor=white)

# Gentoo Dotfiles

Personal Gentoo Linux configuration with shell script for system setup and chezmoi for dotfiles.

## Features

- **Setup Script**: Simple bash script for package installation
- **Dotfiles**: Managed with chezmoi
- **Overlays**: guru and xlibre
- **X Server**: Xlibre (community fork of Xorg)
- **Browsers**: Librewolf, Brave, Helium
- **Development**: Node.js, Python, Go, Rust
- **Kubernetes**: kubectl, helm, k9s, kubie, stern
- **Containers**: Podman (rootless), libvirt/QEMU

## Quick Start

```bash
# 1. Bootstrap chezmoi and dotfiles
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply Saulimedes/dotfiles

# 2. Run setup script
cd ~/.local/share/chezmoi
./setup.sh all

# 3. Reboot and enjoy
```

## Selective Installation

```bash
./setup.sh portage base     # Minimal: overlays + CLI tools
./setup.sh desktop          # Desktop environment, fonts, audio
./setup.sh dev              # Development tools
./setup.sh work             # Kubernetes/cloud
./setup.sh browsers         # Librewolf, Brave, Helium
./setup.sh virt             # Podman, libvirt
./setup.sh services         # Systemd user services
```

## Structure

```
.
├── setup.sh                    # System setup script
│
├── dot_config/                 # Application configs
│   ├── nvim/                   # Neovim
│   ├── kitty/                  # Kitty terminal
│   ├── starship.toml           # Prompt
│   └── atuin/                  # Shell history
│
├── dot_emacs.d/                # Emacs configuration
│   ├── init.el
│   ├── early-init.el
│   └── lisp/
│
├── dot_zsh/                    # Zsh plugins
├── dot_zshrc.tmpl              # Zsh config
├── dot_gitconfig.tmpl          # Git config
└── dot_tmux.conf               # Tmux config
```

## Setup Commands

| Command | Description |
|---------|-------------|
| `portage` | Configure overlays (guru, xlibre) |
| `base` | Core CLI tools (zsh, bat, eza, fzf, etc.) |
| `desktop` | Desktop environment, Xlibre, fonts, PipeWire, media |
| `dev` | Languages and build tools |
| `work` | Kubernetes and cloud tools |
| `browsers` | Librewolf, Brave, Helium |
| `virt` | Podman, libvirt/QEMU |
| `services` | Systemd user services |
| `kernel` | Kernel build dependencies |
| `all` | Everything |

## Xlibre

Uses [Xlibre](https://wiki.gentoo.org/wiki/Xlibre), a community fork of Xorg.

**Note**: Nvidia proprietary drivers are incompatible with Xlibre. Use nouveau or AMD/Intel drivers.

## Browsers

- **Librewolf**: Privacy-focused Firefox fork (default)
- **Brave**: Chromium-based with ad blocking
- **Helium**: Privacy-focused Chromium fork (from guru overlay)

## Troubleshooting

### Overlay issues
```bash
sudo eselect repository enable guru
sudo eselect repository enable xlibre
sudo emaint sync -a
```

### Package masked
```bash
echo "category/package ~amd64" | sudo tee -a /etc/portage/package.accept_keywords/custom
```

### Rootless podman
```bash
echo "$USER:100000:65536" | sudo tee -a /etc/subuid
echo "$USER:100000:65536" | sudo tee -a /etc/subgid
```

## License

GPLv3
