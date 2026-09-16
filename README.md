<p align="center">
  <img src=".github/images/dotfile_image.svg" alt="Dotfiles" width="430">
</p>

<p align="center">
  <a href="https://github.com/Saulimedes/dotfiles/actions/workflows/lint.yml">
  <img src="https://github.com/Saulimedes/dotfiles/actions/workflows/lint.yml/badge.svg" alt="Lint">
  <img src="https://img.shields.io/badge/license-GPLv3-green?logo=gplv3" alt="License">
  <img src="https://img.shields.io/badge/distro-Gentoo-54487A?logo=gentoo&logoColor=white" alt="Gentoo">
  <img src="https://img.shields.io/badge/editor-emacs-7F5AB6?logo=gnuemacs&logoColor=white" alt="Emacs">
  <img src="https://img.shields.io/badge/shell-zsh-F15A24?logo=zsh&logoColor=white" alt="Zsh">
  <img src="https://img.shields.io/badge/dotfiles-chezmoi-285577?logo=googledocs&logoColor=white" alt="chezmoi">
</p>

# Dotfiles

Personal Gentoo Linux dotfiles managed with [chezmoi](https://chezmoi.io).

## Quick Start

```bash
# Bootstrap chezmoi and apply dotfiles
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply Saulimedes/dotfiles
```

You'll be prompted once for your name and email (used for git/jj config, kept out of this repo).

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
- Browsers: Brave, Helium
- Messaging: Telegram, Session

**mise** (`dot_config/mise/config.toml`):
- Languages: Node, Python, Go, Rust, Zig, Java, Bun, Deno
- Kubernetes: kubectl, helm, k9s, stern, kustomize, k3s
- IaC/Cloud: OpenTofu, Vault, Ansible, gcloud, azure-cli
- Dev tools: direnv, gh, jq, yq, shellcheck, shfmt

### Atomic Setup Scripts

| Script | Purpose |
|--------|---------|
| `run_once_setup-portage.sh` | Enable Gentoo overlays (guru, saulimedes-overlay, another-brave-overlay, pentoo) |
| `run_once_setup-flatpak.sh` | Configure Flathub, install Flatpak apps |
| `run_once_install-packages.sh` | Install packages from `packages.txt` |
| `run_once_install-antidote.sh` | Install zsh plugin manager |
| `run_once_install_tmux_plugins` | Install TPM and plugins |
| `run_once_install-mpv-plugins.sh` | Install mpv scripts/shaders (mpv360, gif-generator, webm) |
| `run_once_setup-podman.sh` | Configure rootless podman (subuid/subgid, docker symlink) |
| `run_once_setup-xkb-udev-rule.sh` | udev rule to re-apply the XKB patch on keyboard hotplug |
| `run_onchange_mise-install` | Install mise tools |
| `run_onchange_install-packages.sh` | Re-installs packages when `packages.txt` changes |
| `run_onchange_setup-openrc-services.sh` | Enable OpenRC services (chronyd, tlp, bluetooth, mullvad-daemon, i2pd, autofs, syncthing) |
| `run_onchange_setup-samba-mounts.sh` | Configure autofs mounts to the Unraid NAS |
| `run_onchange_sync-genkernel.sh.tmpl` | Sync `genkernel.conf` to `/etc/genkernel.conf` |
| `run_onchange_sync-useflags.sh.tmpl` | Sync `useflags.txt` to `/etc/portage/package.use/` |

### Shell Configuration

- **Zsh** with [antidote](https://getantidote.github.io/) plugin manager
- **Starship** prompt
- **Atuin** shell history
- **forgit** for fzf + git integration
- Custom abbreviations and functions

### Keyboard Layout

- **US QWERTY** with `altgr-weur` variant for Western European characters
- `;` key remapped to `dead_diaeresis` via XKB — type `;a` for ä, `;o` for ö, `;s` for ẞ
- `;;` for literal semicolon, `; ` (space) preserves natural typing flow
- AltGr+letter still available for direct accented characters (hold AltGr+a → ä, AltGr+8 → ß)
- Custom `.XCompose` overrides for ẞ and semicolon fallback sequences
- Caps Lock → Control via `ctrl:nocaps`

### Editor

- **Emacs** with custom config (`dot_emacs.d/`)
- `emacsclient` as default editor
- Dired alias: `d` opens current directory

## Mise Tasks

| Task | Purpose |
|------|---------|
| `mise run kernel` | Copy saved config, build kernel, install modules, rebuild initramfs, update limine.conf |
| `mise run kernel-config-save` | Save `/usr/src/linux/.config` back to chezmoi (`~/.config/kernel/linux.config`) |

**Kernel upgrade workflow:**
```bash
eselect kernel set <new>
mise run kernel                          # applies saved config, builds
# if config needs updating for new kernel:
sudo make -C /usr/src/linux olddefconfig
mise run kernel-config-save              # save updated config
git -C ~/.local/share/chezmoi add dot_config/kernel/linux.config && git commit
mise run kernel
```

## Re-running Scripts

```bash
# Re-run all setup scripts
chezmoi state delete-bucket --bucket=scriptState
chezmoi apply

# Re-run a specific script manually
chezmoi execute-template < run_onchange_install-packages.sh.tmpl | bash
chezmoi execute-template < run_onchange_sync-useflags.sh.tmpl | bash
```

## Requirements

- Gentoo Linux with `~amd64` in ACCEPT_KEYWORDS
- Overlays: guru, saulimedes-overlay, another-brave-overlay, pentoo (auto-configured)

## License

GPLv3
