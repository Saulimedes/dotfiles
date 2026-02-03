<p align="center">
  <img src=".github/images/dotfile_image.png" alt="Dotfiles" width="600">
</p>

<p align="center">
  <img src="https://img.shields.io/badge/license-GPLv3-green.svg" alt="License">
  <img src="https://img.shields.io/badge/distro-Gentoo-54487A?logo=gentoo&logoColor=white" alt="Gentoo">
  <img src="https://img.shields.io/badge/editor-emacs-7F5AB6?logo=gnuemacs&logoColor=white" alt="Emacs">
  <img src="https://img.shields.io/badge/shell-zsh-green?logo=zsh&logoColor=white" alt="Zsh">
  <img src="https://img.shields.io/badge/dotfiles-chezmoi-blue" alt="chezmoi">
</p>

# Dotfiles

Personal Gentoo Linux dotfiles managed with [chezmoi](https://chezmoi.io).

## Quick Start

```bash
# Bootstrap chezmoi and apply dotfiles
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply Saulimedes/dotfiles
```

This will:
1. Install chezmoi
2. Clone dotfiles
3. Run atomic setup scripts (overlays, packages, flatpak, etc.)
4. Apply all configuration files

## What's Included

### Package Management

**Gentoo packages** (`packages.txt`):
- Shells: zsh, starship, atuin, zoxide
- CLI tools: bat, eza, fd, ripgrep, fzf
- Network: nmap, mtr, tcpdump, iperf
- Media: mpv, ffmpeg
- Browsers: Librewolf, Helium, Zen Browser
- Messaging: Telegram, Session

**mise** (`dot_config/mise/config.toml`):
- Languages: Node, Python, Go, Rust, Zig, Java, Bun, Deno
- Kubernetes: kubectl, helm, k9s, kubie, stern, kustomize, k3s
- IaC/Cloud: OpenTofu, Vault, Ansible, gcloud, azure-cli
- Dev tools: direnv, gh, jq, yq, shellcheck, shfmt

### Atomic Setup Scripts

| Script | Purpose |
|--------|---------|
| `run_once_setup-portage.sh` | Enable Gentoo overlays (guru, librewolf) |
| `run_once_setup-flatpak.sh` | Configure Flathub |
| `run_once_install-packages.sh` | Install packages from `packages.txt` |
| `run_once_install-antidote.sh` | Install zsh plugin manager |
| `run_once_install_tmux_plugins` | Install TPM and plugins |
| `run_once_setup-systemd-services.sh` | Enable user services |
| `run_onchange_mise-install` | Install mise tools |

### Shell Configuration

- **Zsh** with [antidote](https://getantidote.github.io/) plugin manager
- **Starship** prompt
- **Atuin** shell history
- **forgit** for fzf + git integration
- Custom abbreviations and functions

### Editor

- **Emacs** with custom config (`dot_emacs.d/`)
- `emacsclient` as default editor
- Dired alias: `d` opens current directory

## Structure

```
.
├── packages.txt              # Gentoo packages
├── run_once_*.sh             # Atomic setup scripts
├── dot_config/
│   ├── mise/config.toml      # mise tools
│   ├── kitty/                # Terminal
│   ├── starship.toml         # Prompt
│   └── ...
├── dot_emacs.d/              # Emacs config
├── dot_zsh/                  # Zsh plugins & functions
├── dot_zshrc.tmpl            # Zsh config (templated)
├── dot_gitconfig.tmpl        # Git config (templated)
└── dot_tmux.conf             # Tmux config
```

## Manual Installation

```bash
# Install specific packages manually
./run_once_install-packages.sh

# Install mise tools
mise install
```

## Adding Packages

Edit `packages.txt` and run:
```bash
./run_once_install-packages.sh
```

Edit `dot_config/mise/config.toml` and run:
```bash
mise install
```

## Requirements

- Gentoo Linux with `~amd64` in ACCEPT_KEYWORDS
- Overlays: guru, librewolf (auto-configured)

## License

GPLv3
