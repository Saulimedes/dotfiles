# Editor configurations
{ config, pkgs, lib, ... }:

{
  imports = [
    ./neovim
    ./emacs
  ];
}
