# CLI tool configurations
{ config, pkgs, lib, ... }:

{
  imports = [
    ./git.nix
    ./atuin.nix
    ./starship.nix
    ./direnv.nix
    ./fzf.nix
    ./file-tools.nix
    ./kubernetes.nix
    ./development.nix
    ./misc.nix
    ./btop.nix
  ];

  # Common CLI packages without specific configuration
  home.packages = with pkgs; [
    # Archives
    zip
    unzip
    p7zip
    unrar

    # Networking
    curl
    wget
    httpie
    nmap

    # System monitoring
    duf
    ncdu

    # Text processing
    jq
    yq-go

    # Misc utilities
    tree
    file
    which
    gnused
    gawk
    gnumake

    # Containers
    distrobox
  ];
}
