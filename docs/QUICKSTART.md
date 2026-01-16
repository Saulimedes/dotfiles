# Quick Start Reference

## TL;DR - New Machine Setup

```bash
# 1. Boot NixOS USB, partition disk, mount to /mnt

# 2. Clone repo
sudo nix-shell -p git
sudo git clone https://github.com/YOUR_USER/dotfiles.git /mnt/etc/nixos

# 3. Generate hardware config
sudo nixos-generate-config --root /mnt --show-hardware-config > /mnt/etc/nixos/hosts/HOSTNAME/hardware-configuration.nix

# 4. Edit flake.nix - add your host to nixosConfigurations

# 5. Install
sudo nixos-install --flake /mnt/etc/nixos#HOSTNAME

# 6. Reboot and enjoy
```

---

## Directory Cheatsheet

| Path | Purpose |
|------|---------|
| `flake.nix` | Main entry - edit userConfig and hosts here |
| `hosts/<name>/default.nix` | Machine-specific config (GPU, hostname) |
| `hosts/<name>/hardware-configuration.nix` | Auto-generated, don't edit manually |
| `system/` | NixOS system modules (shared across hosts) |
| `home/` | Home-manager user config |
| `home/modules/shells/fish.nix` | Shell config, aliases, abbrs |
| `home/modules/cli/git.nix` | Git config and aliases |
| `home/modules/editors/neovim/` | Neovim + lua configs |
| `fonts/` | Private fonts (git submodule) |

---

## Common Commands

```bash
# Rebuild system
sudo nixos-rebuild switch --flake /etc/nixos#HOSTNAME

# Rebuild (test without switching)
sudo nixos-rebuild test --flake /etc/nixos#HOSTNAME

# Update all flake inputs
cd /etc/nixos && sudo nix flake update

# Update single input
sudo nix flake lock --update-input nixpkgs

# Rollback to previous generation
sudo nixos-rebuild switch --rollback

# List generations
sudo nix-env --list-generations --profile /nix/var/nix/profiles/system

# Garbage collect old generations
sudo nix-collect-garbage -d

# Check flake syntax
nix flake check

# Enter dev shell (for nix tooling)
nix develop
```

---

## Adding a New Host

1. Create directory:
   ```bash
   mkdir -p hosts/newhost
   ```

2. Generate hardware config on target machine:
   ```bash
   nixos-generate-config --show-hardware-config > hosts/newhost/hardware-configuration.nix
   ```

3. Create `hosts/newhost/default.nix`:
   ```nix
   { config, pkgs, lib, ... }:
   {
     imports = [ ./hardware-configuration.nix ];
     networking.hostName = "newhost";
     system.stateVersion = "24.11";
   }
   ```

4. Add to `flake.nix`:
   ```nix
   nixosConfigurations = {
     newhost = mkHost { hostname = "newhost"; };
   };
   ```

5. Build:
   ```bash
   sudo nixos-rebuild switch --flake .#newhost
   ```

---

## Customization Quick Reference

### Change default shell
Edit `system/users.nix`:
```nix
users.users.${userConfig.username}.shell = pkgs.fish;  # or pkgs.zsh
```

### Add system packages
Edit `system/default.nix`:
```nix
environment.systemPackages = with pkgs; [ ... ];
```

### Add user packages
Edit `home/modules/cli/default.nix` or create new module:
```nix
home.packages = with pkgs; [ ... ];
```

### Change desktop environment
Edit `system/desktop.nix`:
```nix
# GNOME (default)
services.xserver.displayManager.gdm.enable = true;
services.xserver.desktopManager.gnome.enable = true;

# KDE Plasma
services.displayManager.sddm.enable = true;
services.desktopManager.plasma6.enable = true;

# Hyprland
programs.hyprland.enable = true;
```

### Add shell aliases
Edit `home/modules/shells/fish.nix`:
```nix
programs.fish.shellAliases = {
  myalias = "my command";
};
```

### Add git aliases
Edit `home/modules/cli/git.nix`:
```nix
programs.git.aliases = {
  myalias = "my git command";
};
```

---

## Troubleshooting

| Problem | Solution |
|---------|----------|
| Flakes not enabled | `export NIX_CONFIG="experimental-features = nix-command flakes"` |
| Build fails | Check syntax: `nix flake check` |
| Package not found | Search: `nix search nixpkgs packagename` |
| Rollback needed | `sudo nixos-rebuild switch --rollback` |
| Fish not default | Re-login or `chsh -s $(which fish)` |
| Fonts not showing | Run `fc-cache -fv` then restart apps |
