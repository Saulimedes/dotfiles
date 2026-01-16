# Home Manager configuration entry point
{ config, pkgs, lib, userConfig, ... }:

{
  imports = [
    ./modules/shells
    ./modules/editors
    ./modules/terminals
    ./modules/cli
    ./modules/services
    ./modules/scripts
    ./modules/fonts.nix
    ./modules/browsers.nix
  ];

  # Home Manager state version
  home.stateVersion = "24.11";

  # Let Home Manager manage itself
  programs.home-manager.enable = true;

  # User info
  home.username = userConfig.username;
  home.homeDirectory = "/home/${userConfig.username}";

  # Environment variables
  home.sessionVariables = {
    EDITOR = userConfig.editor;
    VISUAL = userConfig.visual;
    MANPAGER = userConfig.manpager;
  };

  # Additional paths
  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/.krew/bin"
    "$HOME/.cargo/bin"
    "$HOME/.npm-global/bin"
  ];

  # XDG directories
  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
    };
  };

  # Cursor theme
  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
    size = 24;
  };

  # GTK theming
  gtk = {
    enable = true;
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome-themes-extra;
    };
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
  };
}
