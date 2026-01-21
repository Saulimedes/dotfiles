# Stylix theming configuration (base settings)
# Colors/cursor are set per-host via profiles/themes/
{ config, pkgs, lib, ... }:

{
  stylix = {
    enable = true;

    # Wallpaper (required by Stylix - using solid color from base00)
    image = pkgs.runCommand "wallpaper.png" { } ''
      ${pkgs.imagemagick}/bin/convert -size 1920x1080 xc:#000000 $out
    '';

    # Fonts (shared across all hosts)
    fonts = {
      monospace = {
        # MonoLisa installed via private-fonts in home/modules/fonts.nix
        # Using null package since font is already installed via home.file
        package = pkgs.emptyDirectory;
        name = "MonoLisaVariable Nerd Font";
      };
      sansSerif = {
        package = pkgs.inter;
        name = "Inter";
      };
      serif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
      emoji = {
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };
      sizes = {
        applications = 11;
        desktop = 11;
        popups = 11;
        terminal = 13;
      };
    };

    opacity = {
      applications = 1.0;
      desktop = 1.0;
      popups = 1.0;
      terminal = 0.95;
    };

    targets = {
      gtk.enable = true;
      qt.enable = true;
      # These are auto-enabled when the programs are enabled:
      # - kitty (programs.kitty.enable)
      # - btop (programs.btop.enable)
      # - neovim (programs.neovim.enable) - generates base16-stylix colorscheme
    };
  };
}
