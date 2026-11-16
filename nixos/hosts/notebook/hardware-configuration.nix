# placeholder, install.sh overwrites it with nixos-generate-config output
{ lib, ... }:
{
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
