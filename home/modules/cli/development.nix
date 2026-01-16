# Development tools
{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    # Languages (via nixpkgs instead of mise for reproducibility)
    nodejs_22
    python3
    go
    rustup
    deno
    bun

    # Build tools
    cmake
    ninja
    meson

    # Package managers
    yarn
    pnpm

    # Cloud tools
    awscli2
    azure-cli
    google-cloud-sdk

    # IaC
    opentofu
    terraform
    ansible

    # Database tools
    postgresql
    sqlite

    # API development
    insomnia
    postman

    # Misc dev tools
    gh # GitHub CLI (also in git.nix but listed here for visibility)
    pre-commit
    shellcheck
    shfmt
  ];

  # Mise (if you prefer mise over nixpkgs for version management)
  # home.file.".config/mise/config.toml".text = ''
  #   [tools]
  #   node = "latest"
  #   python = "latest"
  #   go = "latest"
  # '';
}
