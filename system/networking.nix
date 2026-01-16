# Networking configuration
{ config, pkgs, lib, ... }:

{
  # NetworkManager for easy network management
  networking.networkmanager.enable = true;

  # Firewall
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22000 # Syncthing
    ];
    allowedUDPPorts = [
      22000 # Syncthing
      21027 # Syncthing discovery
    ];
  };

  # Enable mDNS
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  # SSH server
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };
}
