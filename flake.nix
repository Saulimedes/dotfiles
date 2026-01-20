{
  description = "NixOS configuration with Home Manager";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Hardware support
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    # Emacs overlay for latest packages
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Stylix for consistent theming
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Nix User Repository (for Firefox extensions, etc.)
    nur.url = "github:nix-community/NUR";

    # Private fonts repository
    # Fetch as user first: nix flake update private-fonts
    # Then sudo nixos-rebuild switch
    private-fonts = {
      url = "git+ssh://git@github.com/Saulimedes/fonts.git";
      flake = false;
    };

    # XLibre (X.org community fork)
    xlibre-overlay.url = "git+https://codeberg.org/takagemacoed/xlibre-overlay";
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";

      # User configuration (replaces chezmoi.yml data)
      userConfig = {
        username = "becker";
        fullName = "Paul Becker";
        email = "p@becker.kiwi";
        editor = "nvim";
        visual = "emacsclient -c -a emacs";
        manpager = "less -R";
      };

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          inputs.emacs-overlay.overlays.default
          inputs.nur.overlays.default
          (import ./overlays)
        ];
      };

      # Helper function for creating NixOS configurations
      mkHost = { hostname, enableWork ? false, theme ? "nord-dark", extraModules ? [ ] }:
        let
          hostConfig = {
            inherit hostname enableWork theme;
          };
          # Map theme name to profile path
          themeModule = ./home/modules/profiles/themes/${theme}.nix;
        in
        nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs userConfig hostConfig; };
          modules = [
          # Apply overlays to nixpkgs
          {
            nixpkgs.overlays = [
              inputs.emacs-overlay.overlays.default
              inputs.nur.overlays.default
              (import ./overlays)
            ];
            nixpkgs.config.allowUnfree = true;
          }
          ./hosts/${hostname}
          ./system
          # Per-host theme (colors, cursor, polarity)
          themeModule
          # XLibre X server (replaces xorg.xorgserver)
          inputs.xlibre-overlay.nixosModules.overlay-xlibre-xserver
          inputs.xlibre-overlay.nixosModules.overlay-all-xlibre-drivers
          # Stylix theming
          inputs.stylix.nixosModules.stylix
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              backupFileExtension = "backup";
              users.${userConfig.username} = import ./home;
              extraSpecialArgs = { inherit inputs userConfig hostConfig; };
            };
          }
        ] ++ extraModules;
      };

    in
    {
      nixosConfigurations = {
        # Each host can have a different theme
        nitipa1 = mkHost {
          hostname = "nitipa1";
          theme = "nord-dark";  # Options: nord-dark, catppuccin-mocha, gruvbox-dark
        };
        # Example: another machine with different theme
        # workstation = mkHost {
        #   hostname = "workstation";
        #   theme = "catppuccin-mocha";
        #   enableWork = true;
        # };
      };

      # Development shell for working with this flake
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          nil # Nix LSP
          nixpkgs-fmt
          statix
          deadnix
        ];
      };
    };
}
