{ lib, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./disko.nix

    ../../modules/desktop.nix
    ../../modules/laptop.nix
    ../../modules/podman.nix
    ../../modules/vpn.nix
    ../../modules/syncthing.nix
    # ../../modules/pentest.nix
  ] ++ lib.optional (builtins.pathExists ./disk-device.nix) ./disk-device.nix;
}
