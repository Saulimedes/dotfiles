# Custom packages
{ pkgs }:

{
  # Helium Browser - Chromium-based browser with MV2 support
  helium = pkgs.callPackage ./helium.nix { };

  # Add more custom packages here as needed
  # my-app = pkgs.callPackage ./my-app.nix { };
}
