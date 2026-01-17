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
    ./modules/desktop
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
    VISUAL = lib.mkForce userConfig.visual;  # Override neovim's default
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
      extraConfig = {
        XDG_PROJECTS_DIR = "${config.home.homeDirectory}/Projects";
      };
    };
  };

  # Create additional directories
  systemd.user.tmpfiles.rules = [
    "d %h/Projects 0755 - - -"
    "d %h/Documents/org 0755 - - -"
    "d %h/Pictures/tmp 0755 - - -"
  ];

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
