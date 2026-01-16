# NixOS Configuration

Personal NixOS + Home Manager configuration using Nix Flakes.

## Documentation

- **[Bootstrap Guide](docs/BOOTSTRAP.md)** - Complete guide for setting up a new NixOS machine
- **[Quick Start](docs/QUICKSTART.md)** - Cheatsheet and common commands

## Structure

```
.
├── flake.nix              # Main flake entry point
├── flake.lock             # Locked dependencies
│
├── hosts/                 # Per-machine NixOS configurations
│   └── nixos/             # Default host (rename to your hostname)
│       ├── default.nix
│       └── hardware-configuration.nix
│
├── system/                # NixOS system-level modules
│   ├── default.nix        # Main system config
│   ├── boot.nix           # Bootloader configuration
│   ├── networking.nix     # Network settings
│   ├── users.nix          # User accounts
│   ├── desktop.nix        # Desktop environment
│   ├── audio.nix          # Audio (PipeWire)
│   └── virtualisation.nix # Docker, Podman, VMs
│
├── home/                  # Home Manager configuration
│   ├── default.nix        # Home entry point
│   └── modules/
│       ├── shells/        # Fish, Bash configuration
│       ├── editors/       # Neovim, Emacs
│       ├── terminals/     # Kitty, Ghostty, Tmux
│       ├── cli/           # CLI tools (git, fzf, etc.)
│       ├── services/      # User services (syncthing)
│       ├── scripts/       # Custom shell scripts
│       └── fonts.nix      # Font configuration
│
├── fonts/                 # Private fonts (git submodule)
└── files/                 # Static config files
```

## Quick Start

### 1. Generate hardware configuration

On your NixOS machine:

```bash
nixos-generate-config --show-hardware-config > hosts/nixos/hardware-configuration.nix
```

### 2. Update hostname

Edit `hosts/nixos/default.nix` and `flake.nix` to match your hostname.

### 3. Add fonts submodule (optional)

```bash
git submodule add <your-fonts-repo-url> fonts
```

### 4. Build and switch

```bash
# Build without activating (test)
nixos-rebuild build --flake .#nixos

# Build and activate
sudo nixos-rebuild switch --flake .#nixos
```

## Commands

```bash
# Rebuild system
sudo nixos-rebuild switch --flake .#nixos

# Update flake inputs
nix flake update

# Update single input
nix flake lock --update-input home-manager

# Check flake
nix flake check

# Enter dev shell
nix develop

# Garbage collection
sudo nix-collect-garbage -d
```

## Customization

### Adding a new host

1. Create `hosts/<hostname>/default.nix`
2. Generate `hardware-configuration.nix` on the target machine
3. Add to `flake.nix`:
   ```nix
   nixosConfigurations.<hostname> = mkHost { hostname = "<hostname>"; };
   ```

### User configuration

Edit `flake.nix` to update user details:
```nix
userConfig = {
  username = "your-username";
  fullName = "Your Name";
  email = "your@email.com";
  ...
};
```
