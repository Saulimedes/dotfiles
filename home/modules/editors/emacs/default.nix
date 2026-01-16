# Emacs configuration
{ config, pkgs, lib, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk; # GTK + native compilation

    extraPackages = epkgs: with epkgs; [
      # Essential packages
      use-package
      which-key
      general
      evil
      evil-collection

      # Completion
      vertico
      orderless
      marginalia
      consult
      corfu
      cape

      # UI
      doom-themes
      doom-modeline
      all-the-icons

      # Git
      magit
      git-gutter

      # Programming
      lsp-mode
      lsp-ui
      flycheck
      company
      yasnippet

      # Languages
      nix-mode
      markdown-mode
      yaml-mode
      json-mode
      rust-mode
      go-mode

      # Org mode
      org
      org-bullets

      # Utilities
      vterm
      projectile
      treemacs
      undo-tree
    ];
  };

  # Copy emacs config files (if you have existing elisp configs)
  # xdg.configFile."emacs" = {
  #   source = ./config;
  #   recursive = true;
  # };

  # Or use home.file for ~/.emacs.d
  # home.file.".emacs.d" = {
  #   source = ./config;
  #   recursive = true;
  # };
}
