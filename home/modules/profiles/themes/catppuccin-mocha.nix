# Catppuccin Mocha theme profile
{ config, pkgs, lib, ... }:

{
  stylix = {
    # Catppuccin Mocha
    base16Scheme = {
      base00 = "1e1e2e"; # Base
      base01 = "181825"; # Mantle
      base02 = "313244"; # Surface0
      base03 = "45475a"; # Surface1
      base04 = "585b70"; # Surface2
      base05 = "cdd6f4"; # Text
      base06 = "f5e0dc"; # Rosewater
      base07 = "b4befe"; # Lavender
      base08 = "f38ba8"; # Red
      base09 = "fab387"; # Peach
      base0A = "f9e2af"; # Yellow
      base0B = "a6e3a1"; # Green
      base0C = "94e2d5"; # Teal
      base0D = "89b4fa"; # Blue
      base0E = "cba6f7"; # Mauve
      base0F = "f2cdcd"; # Flamingo
    };

    polarity = "dark";

    cursor = {
      package = pkgs.custom.google-cursor;
      name = "GoogleDot-Blue";
      size = 24;
    };
  };
}
