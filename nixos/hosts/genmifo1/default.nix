{ ... }:
{
  imports = [
    ./hardware-configuration.nix

    ../../modules/desktop.nix
    ../../modules/podman.nix
    ../../modules/vpn.nix
    ../../modules/syncthing.nix
  ];
}
