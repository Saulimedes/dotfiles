# Host-specific configuration for nitipa1
{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # Hostname
  networking.hostName = "nitipa1";

  # Swap (4GB swapfile)
  swapDevices = [{
    device = "/swapfile";
    size = 4096;  # MB
  }];

  # System state version - DO NOT change after initial install
  system.stateVersion = "25.11";
}
