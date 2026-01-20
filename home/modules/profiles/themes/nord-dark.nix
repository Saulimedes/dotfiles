# Nord Dark theme profile
{ config, pkgs, lib, ... }:

{
  stylix = {
    # Nord with pitch black background
    base16Scheme = {
      base00 = "000000"; # Background - pitch black
      base01 = "3b4252"; # Lighter background
      base02 = "434c5e"; # Selection background
      base03 = "4c566a"; # Comments
      base04 = "d8dee9"; # Dark foreground
      base05 = "e5e9f0"; # Foreground
      base06 = "eceff4"; # Light foreground
      base07 = "8fbcbb"; # Cyan
      base08 = "bf616a"; # Red
      base09 = "d08770"; # Orange
      base0A = "ebcb8b"; # Yellow
      base0B = "a3be8c"; # Green
      base0C = "88c0d0"; # Cyan
      base0D = "81a1c1"; # Blue
      base0E = "b48ead"; # Purple
      base0F = "5e81ac"; # Dark blue
    };

    polarity = "dark";

    cursor = {
      package = pkgs.custom.future-cursors;
      name = "Future-dark-cursors";
      size = 24;
    };
  };
}
