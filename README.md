# NixOS Configuration

Personal NixOS + Home Manager configuration using Nix Flakes.

## Quick Start (New Machine)

### 1. Boot NixOS installer and install minimal system

```bash
# Partition, format, mount as needed, then:
nixos-generate-config --root /mnt
nixos-install
reboot
```

### 2. Clone this repository

```bash
nix-shell -p git
git clone git@github.com:Saulimedes/nixos-config.git ~/nixos-config
cd ~/nixos-config
```

### 3. Set up your host

```bash
# Copy your hardware configuration
cp /etc/nixos/hardware-configuration.nix hosts/nitipa1/

# Or create a new host:
mkdir -p hosts/<your-hostname>
nixos-generate-config --show-hardware-config > hosts/<your-hostname>/hardware-configuration.nix
```

If creating a new host, add it to `flake.nix`:
```nix
nixosConfigurations = {
  nitipa1 = mkHost { hostname = "nitipa1"; };
  <your-hostname> = mkHost { hostname = "<your-hostname>"; };
};
```

### 4. Enable flakes (if not already)

```bash
# Temporarily enable flakes for first build
export NIX_CONFIG="experimental-features = nix-command flakes"
```

### 5. Fetch private inputs and build

```bash
# Fetch private repos (fonts) as your user (has SSH keys)
nix flake update private-fonts

# Build and switch
sudo nixos-rebuild switch --flake .#nitipa1
```

## Daily Usage

```bash
# Rebuild after changes
sudo nixos-rebuild switch --flake .#nitipa1

# Update all flake inputs
nix flake update
sudo nixos-rebuild switch --flake .#nitipa1

# Update single input (e.g., fonts)
nix flake update private-fonts
sudo nixos-rebuild switch --flake .#nitipa1

# Test build without activating
nixos-rebuild build --flake .#nitipa1

# Garbage collection
sudo nix-collect-garbage -d
nix-collect-garbage -d  # user store
```

## Structure

```
.
├── flake.nix              # Main flake entry point
├── flake.lock             # Locked dependencies
│
├── hosts/                 # Per-machine configurations
│   └── nitipa1/
│       ├── default.nix    # Host-specific settings
│       └── hardware-configuration.nix
│
├── system/                # NixOS system modules
│   ├── default.nix        # Main config (locale, nix settings)
│   ├── boot.nix           # Bootloader
│   ├── networking.nix     # Network, SSH, firewall
│   ├── users.nix          # User accounts
│   ├── desktop.nix        # XFCE, fonts, flatpak
│   ├── audio.nix          # PipeWire, Bluetooth
│   └── virtualisation.nix # Docker, Podman, libvirt
│
├── home/                  # Home Manager configuration
│   ├── default.nix        # Entry point
│   └── modules/
│       ├── shells/        # Fish (primary), Bash
│       ├── editors/       # Neovim, Emacs
│       ├── terminals/     # Kitty, Ghostty, Tmux
│       ├── cli/           # Git, fzf, starship, atuin, etc.
│       ├── services/      # Syncthing, GPG agent
│       ├── scripts/       # Custom scripts (extract, ydl, etc.)
│       ├── browsers.nix   # Brave, Firefox, Zen, Chromium
│       └── fonts.nix      # Fonts (nixpkgs + private)
│
├── pkgs/                  # Custom packages
│   └── helium.nix         # Helium browser (disabled)
│
├── overlays/              # Nixpkgs overlays
│
└── fonts/                 # Private fonts (git submodule, fetched via flake input)
```

## Customization

### User Configuration

Edit `flake.nix`:
```nix
userConfig = {
  username = "becker";
  fullName = "Paul Becker";
  email = "p@becker.kiwi";
  editor = "nvim";
  visual = "emacsclient -c -a emacs";
  manpager = "less -R";
};
```

### Adding a New Host

1. Create host directory:
   ```bash
   mkdir -p hosts/<hostname>
   ```

2. Generate hardware config on target machine:
   ```bash
   nixos-generate-config --show-hardware-config > hosts/<hostname>/hardware-configuration.nix
   ```

3. Create `hosts/<hostname>/default.nix`:
   ```nix
   { config, pkgs, lib, ... }:
   {
     imports = [ ./hardware-configuration.nix ];
     networking.hostName = "<hostname>";
     system.stateVersion = "25.11";  # Set to your NixOS version
   }
   ```

4. Add to `flake.nix`:
   ```nix
   nixosConfigurations.<hostname> = mkHost { hostname = "<hostname>"; };
   ```

### Private Fonts

Fonts are fetched from a private GitHub repo via SSH. To update:

```bash
# As regular user (has SSH keys)
nix flake update private-fonts

# Then rebuild
sudo nixos-rebuild switch --flake .#nitipa1
```

Fonts are installed to `~/.local/share/fonts/private/`.

## Flake Inputs

| Input | Description |
|-------|-------------|
| nixpkgs | NixOS unstable |
| home-manager | Home Manager |
| emacs-overlay | Latest Emacs packages |
| nur | Nix User Repository (Firefox extensions) |
| zen-browser | Zen Browser (Firefox fork) |
| private-fonts | Private fonts repository |

## Troubleshooting

### Permission denied fetching private repos

Private repos (fonts) need SSH access. Run as your user first:
```bash
nix flake update private-fonts
sudo nixos-rebuild switch --flake .#nitipa1
```

### Git permission errors

If you see "insufficient permission for adding an object":
```bash
sudo chown -R $USER:users ~/nixos-config
```

### Dirty git tree warnings

Commit or stash your changes:
```bash
git add -A && git commit -m "WIP"
# or
git stash
```
