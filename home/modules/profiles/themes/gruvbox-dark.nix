# Gruvbox Dark theme profile
{ config, pkgs, lib, ... }:

{
  stylix = {
    # Gruvbox Dark
    base16Scheme = {
      base00 = "1d2021"; # bg0_h (dark)
      base01 = "3c3836"; # bg1
      base02 = "504945"; # bg2
      base03 = "665c54"; # bg3
      base04 = "bdae93"; # fg3
      base05 = "d5c4a1"; # fg2
      base06 = "ebdbb2"; # fg1
      base07 = "fbf1c7"; # fg0
      base08 = "fb4934"; # red
      base09 = "fe8019"; # orange
      base0A = "fabd2f"; # yellow
      base0B = "b8bb26"; # green
      base0C = "8ec07c"; # aqua
      base0D = "83a598"; # blue
      base0E = "d3869b"; # purple
      base0F = "d65d0e"; # brown
    };

    polarity = "dark";

    cursor = {
      package = pkgs.custom.google-cursor;
      name = "GoogleDot-Black";
      size = 24;
    };
  };
}
