# System-level NixOS configuration
{ config, pkgs, lib, userConfig, ... }:

{
  imports = [
    ./boot.nix
    ./networking.nix
    ./users.nix
    ./desktop.nix
    ./audio.nix
    ./virtualisation.nix
    ./theme.nix
  ];

  # Nix settings
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
      trusted-users = [ "root" userConfig.username ];
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Timezone and locale
  time.timeZone = "Europe/Berlin";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "de_DE.UTF-8";
    LC_IDENTIFICATION = "de_DE.UTF-8";
    LC_MEASUREMENT = "de_DE.UTF-8";
    LC_MONETARY = "de_DE.UTF-8";
    LC_NAME = "de_DE.UTF-8";
    LC_NUMERIC = "de_DE.UTF-8";
    LC_PAPER = "de_DE.UTF-8";
    LC_TELEPHONE = "de_DE.UTF-8";
    LC_TIME = "de_DE.UTF-8";
  };

  # System packages available to all users
  environment.systemPackages = with pkgs; [
    vim
    git
    curl
    wget
    htop
    tree
    unzip
    zip
    file
    which
    gnumake
    gcc
  ];

  # Enable firmware updates
  services.fwupd.enable = true;

  # Security
  security.rtkit.enable = true;
  security.polkit.enable = true;
}
