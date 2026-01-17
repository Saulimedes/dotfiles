# Wayland clipboard and utilities
{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    wl-clipboard  # wl-copy, wl-paste
  ];
}
