# Shell configurations
{ config, pkgs, lib, ... }:

{
  imports = [
    ./fish.nix
    ./bash.nix
  ];
}
