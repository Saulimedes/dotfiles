# Neovim configuration
{ config, pkgs, lib, ... }:

{
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    # Runtime dependencies for LSP, formatters, etc.
    extraPackages = with pkgs; [
      # LSP servers
      lua-language-server
      nil # Nix LSP
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted # HTML, CSS, JSON, ESLint
      pyright
      rust-analyzer
      gopls
      terraform-ls
      yaml-language-server
      marksman # Markdown LSP

      # Formatters
      stylua
      nixpkgs-fmt
      black
      isort
      prettierd
      shfmt

      # Linters
      shellcheck

      # Tools used by plugins
      ripgrep
      fd
      tree-sitter
      gcc # For treesitter compilation
      gnumake
    ];
  };

  # Copy lua configuration files to ~/.config/nvim
  # This preserves your existing lazy.nvim setup
  xdg.configFile = {
    "nvim/init.lua".source = ./lua/init.lua;
    "nvim/lua/core" = {
      source = ./lua/core;
      recursive = true;
    };
    "nvim/lua/plugins" = {
      source = ./lua/plugins;
      recursive = true;
    };
  };
}
