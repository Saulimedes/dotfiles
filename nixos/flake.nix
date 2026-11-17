{
  description = "NixOS machines: notipa1, genmifo1";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      disko,
      home-manager,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      mkTask =
        name: text:
        {
          type = "app";
          program = "${pkgs.writeShellScript name text}";
        };
      mkHost =
        hostname:
        {
          system ? "x86_64-linux",
          extraModules ? [ ],
        }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit inputs hostname;
            pkgs-unstable = import nixpkgs-unstable {
              inherit system;
              config.allowUnfree = true;
            };
          };
          modules = [
            disko.nixosModules.disko
            home-manager.nixosModules.home-manager
            ./modules/base.nix
            ./modules/users.nix
            ./modules/home.nix
            ./hosts/${hostname}
          ]
          ++ extraModules;
        };
    in
    {
      nixosConfigurations = {
        notipa1 = mkHost "notipa1" { };
        genmifo1 = mkHost "genmifo1" { };
      };

      apps.${system} = {
        apply = mkTask "apply" ''
          chezmoi apply
        '';
        emacs = mkTask "emacs" ''
          chezmoi apply ~/.emacs.d
          emacs --batch --eval "(progn
            (load user-init-file)
            (straight-pull-all)
            (straight-rebuild-all))"
          echo "[*] Emacs packages updated."
        '';
        fonts = mkTask "fonts" ''
          chezmoi update --include=externals
          fc-cache -fv
          echo "[*] Fonts updated."
        '';
        tmux = mkTask "tmux" ''
          chezmoi apply ~/.tmux.conf
          ~/.tmux/plugins/tpm/bin/update_plugins all
          echo "[*] Tmux plugins updated."
        '';
        pip = mkTask "pip" ''
          pip list --outdated --format=freeze | grep -v '^-e' | cut -d= -f1 | xargs -r pip install --upgrade
          echo "[*] pip packages updated."
        '';
        flatpak = mkTask "flatpak" ''
          flatpak update -y
          echo "[*] Flatpak apps updated."
        '';
        update = mkTask "update" ''
          set -e
          host=$(hostname)
          sudo nixos-rebuild switch --flake "/etc/nixos#$host"
          sudo nix-collect-garbage --delete-older-than 21d
          echo "[*] $host rebuilt and garbage collected."
        '';
      };
    };
}
