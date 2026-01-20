# Optional profiles that can be enabled per-host
{ config, pkgs, lib, ... }:

{
  imports = [
    ./work.nix
  ];
}
