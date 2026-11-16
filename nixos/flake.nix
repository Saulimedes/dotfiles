{
  description = "NixOS machines: notebook, genmifo1";

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
        notebook = mkHost "notebook" { };
        genmifo1 = mkHost "genmifo1" { };
      };
    };
}
