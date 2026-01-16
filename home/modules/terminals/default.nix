# Terminal emulator configurations
{ config, pkgs, lib, ... }:

{
  imports = [
    ./kitty.nix
    ./ghostty.nix
    ./tmux.nix
  ];
}
