# Desktop environment utilities
{ config, pkgs, lib, ... }:

{
  imports = [
    ./xorg.nix
    ./wayland.nix
  ];
}
