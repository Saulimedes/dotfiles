# Bootstrapping a New NixOS System

This guide walks through setting up a fresh NixOS installation with this flake configuration.

## Prerequisites

- USB drive with NixOS ISO (download from https://nixos.org/download)
- Internet connection
- This repository URL or local copy

---

## Step 1: Boot NixOS Installer

1. Boot from the NixOS USB
2. Connect to the internet:
   ```bash
   # For WiFi
   sudo systemctl start wpa_supplicant
   wpa_cli
   > add_network
   > set_network 0 ssid "YourWiFi"
   > set_network 0 psk "YourPassword"
   > enable_network 0
   > quit

   # Or use nmtui for NetworkManager
   sudo nmtui
   ```

---

## Step 2: Partition and Format Disks

### Option A: Simple UEFI Setup (Recommended)

```bash
# Identify your disk (usually /dev/nvme0n1 or /dev/sda)
lsblk

# Partition the disk
sudo parted /dev/nvme0n1 -- mklabel gpt
sudo parted /dev/nvme0n1 -- mkpart ESP fat32 1MB 512MB
sudo parted /dev/nvme0n1 -- set 1 esp on
sudo parted /dev/nvme0n1 -- mkpart primary 512MB 100%

# Format partitions
sudo mkfs.fat -F 32 -n BOOT /dev/nvme0n1p1
sudo mkfs.ext4 -L nixos /dev/nvme0n1p2

# Mount
sudo mount /dev/disk/by-label/nixos /mnt
sudo mkdir -p /mnt/boot
sudo mount /dev/disk/by-label/BOOT /mnt/boot
```

### Option B: With Swap Partition

```bash
sudo parted /dev/nvme0n1 -- mklabel gpt
sudo parted /dev/nvme0n1 -- mkpart ESP fat32 1MB 512MB
sudo parted /dev/nvme0n1 -- set 1 esp on
sudo parted /dev/nvme0n1 -- mkpart primary 512MB -8GB    # Root
sudo parted /dev/nvme0n1 -- mkpart primary linux-swap -8GB 100%  # Swap

sudo mkfs.fat -F 32 -n BOOT /dev/nvme0n1p1
sudo mkfs.ext4 -L nixos /dev/nvme0n1p2
sudo mkswap -L swap /dev/nvme0n1p3

sudo mount /dev/disk/by-label/nixos /mnt
sudo mkdir -p /mnt/boot
sudo mount /dev/disk/by-label/BOOT /mnt/boot
sudo swapon /dev/disk/by-label/swap
```

### Option C: Encrypted Root (LUKS)

```bash
sudo parted /dev/nvme0n1 -- mklabel gpt
sudo parted /dev/nvme0n1 -- mkpart ESP fat32 1MB 512MB
sudo parted /dev/nvme0n1 -- set 1 esp on
sudo parted /dev/nvme0n1 -- mkpart primary 512MB 100%

# Setup encryption
sudo cryptsetup luksFormat /dev/nvme0n1p2
sudo cryptsetup open /dev/nvme0n1p2 cryptroot

sudo mkfs.fat -F 32 -n BOOT /dev/nvme0n1p1
sudo mkfs.ext4 -L nixos /dev/mapper/cryptroot

sudo mount /dev/mapper/cryptroot /mnt
sudo mkdir -p /mnt/boot
sudo mount /dev/disk/by-label/BOOT /mnt/boot
```

---

## Step 3: Clone This Repository

```bash
# Enable flakes in the installer
sudo nix-shell -p git nixFlakes

# Clone the repo
sudo git clone https://github.com/YOUR_USERNAME/dotfiles.git /mnt/etc/nixos

# Or if you have it locally, copy it
# sudo cp -r /path/to/dotfiles /mnt/etc/nixos
```

---

## Step 4: Generate Hardware Configuration

```bash
# Generate hardware config for this specific machine
sudo nixos-generate-config --root /mnt --show-hardware-config > /mnt/etc/nixos/hosts/nixos/hardware-configuration.nix
```

---

## Step 5: Create Host Configuration

### 5.1 Choose a hostname

Pick a name for your machine (e.g., `desktop`, `laptop`, `workstation`).

### 5.2 Create host directory

```bash
HOSTNAME="desktop"  # Change this

# If using a new hostname (not 'nixos')
sudo mkdir -p /mnt/etc/nixos/hosts/$HOSTNAME
sudo mv /mnt/etc/nixos/hosts/nixos/hardware-configuration.nix /mnt/etc/nixos/hosts/$HOSTNAME/
```

### 5.3 Create host default.nix

```bash
sudo cat > /mnt/etc/nixos/hosts/$HOSTNAME/default.nix << 'EOF'
{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # Set your hostname
  networking.hostName = "desktop";  # CHANGE THIS

  # Host-specific settings go here
  # Example: Enable nvidia drivers
  # services.xserver.videoDrivers = [ "nvidia" ];
  # hardware.nvidia.modesetting.enable = true;

  system.stateVersion = "24.11";
}
EOF
```

### 5.4 Update flake.nix

Edit `/mnt/etc/nixos/flake.nix` to add your host:

```nix
nixosConfigurations = {
  # Add your hostname here
  desktop = mkHost { hostname = "desktop"; };

  # You can keep or remove the default 'nixos' entry
  # nixos = mkHost { hostname = "nixos"; };
};
```

### 5.5 Update user configuration

Edit the `userConfig` section in `/mnt/etc/nixos/flake.nix`:

```nix
userConfig = {
  username = "becker";        # Your username
  fullName = "Paul Becker";   # Your full name
  email = "p@becker.kiwi";    # Your email
  editor = "nvim";
  visual = "emacsclient -c -a emacs";
  manpager = "less -R";
};
```

---

## Step 6: Install NixOS

```bash
HOSTNAME="desktop"  # Your chosen hostname

# Install
sudo nixos-install --flake /mnt/etc/nixos#$HOSTNAME

# Set root password when prompted
# Then set user password
sudo nixos-enter --root /mnt -c 'passwd becker'
```

---

## Step 7: Reboot and Finalize

```bash
sudo reboot
```

After reboot:

```bash
# Clone fonts submodule (if you have one)
cd /etc/nixos
sudo git submodule add https://github.com/YOUR_USERNAME/fonts.git fonts
sudo git submodule update --init

# Rebuild to pick up fonts
sudo nixos-rebuild switch --flake /etc/nixos#desktop
```

---

## Quick Reference: Host-Specific Customizations

### For Laptops

Add to `hosts/<hostname>/default.nix`:

```nix
{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "laptop";

  # Laptop-specific
  services.tlp.enable = true;  # Power management
  services.thermald.enable = true;

  # Touchpad
  services.libinput = {
    enable = true;
    touchpad = {
      naturalScrolling = true;
      tapping = true;
    };
  };

  # Backlight control
  programs.light.enable = true;

  system.stateVersion = "24.11";
}
```

### For NVIDIA GPUs

```nix
{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "gaming";

  # NVIDIA
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };
  hardware.graphics.enable = true;

  system.stateVersion = "24.11";
}
```

### For AMD GPUs

```nix
{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "amd-desktop";

  # AMD
  services.xserver.videoDrivers = [ "amdgpu" ];
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      amdvlk
      rocmPackages.clr.icd
    ];
  };

  system.stateVersion = "24.11";
}
```

### For Servers (Headless)

```nix
{ config, pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "server";

  # Disable desktop
  services.xserver.enable = lib.mkForce false;

  # Enable SSH
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # Headless boot
  boot.loader.timeout = 1;

  system.stateVersion = "24.11";
}
```

---

## Troubleshooting

### "experimental feature 'flakes' is disabled"

```bash
# Temporary fix
export NIX_CONFIG="experimental-features = nix-command flakes"

# Or run with flags
sudo nixos-install --flake /mnt/etc/nixos#hostname --extra-experimental-features "nix-command flakes"
```

### Hardware config not detecting all disks

```bash
# Regenerate with all modules loaded
sudo modprobe nvme  # For NVMe drives
sudo nixos-generate-config --root /mnt --show-hardware-config
```

### Boot fails after install

1. Boot from USB again
2. Mount your partitions to /mnt
3. Check logs: `sudo nixos-enter --root /mnt -c 'journalctl -xb'`
4. Fix config and rebuild: `sudo nixos-install --flake /mnt/etc/nixos#hostname`

### Network not working after install

```bash
# Check if NetworkManager is running
systemctl status NetworkManager

# Connect via nmtui
nmtui
```

---

## Post-Install Checklist

- [ ] Set user password: `passwd`
- [ ] Clone fonts submodule (if applicable)
- [ ] Verify fish shell works
- [ ] Test neovim with `:checkhealth`
- [ ] Configure git: already done via home-manager
- [ ] Set up SSH keys
- [ ] Enable syncthing: `systemctl --user start syncthing`
- [ ] Configure displays (if multi-monitor)
