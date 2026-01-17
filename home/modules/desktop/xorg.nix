# X.org clipboard and utilities
{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    xclip
    xsel
  ];
}
