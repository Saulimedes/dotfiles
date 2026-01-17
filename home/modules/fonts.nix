# Font configuration
{ config, pkgs, lib, ... }:

{
  # Fonts from nixpkgs
  home.packages = with pkgs; [
    # Nerd Fonts (new structure in nixpkgs)
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
    nerd-fonts.iosevka-term
    nerd-fonts.fira-code
    nerd-fonts.hack

    # Sans-serif
    inter
    roboto
    source-sans
    ubuntu-classic

    # Serif
    source-serif
    libertinus

    # Monospace
    jetbrains-mono
    fira-code
    source-code-pro

    # Icons and emoji
    font-awesome
    noto-fonts-color-emoji
    material-design-icons

    # CJK (if needed)
    # noto-fonts-cjk-sans

    # Font tools
    fontconfig
  ];

  # Private fonts from git submodule
  # Add submodule with: git submodule add <your-fonts-repo-url> fonts
  home.file.".local/share/fonts/private" = {
    source = ../../fonts;
    recursive = true;
    # onChange triggers fc-cache when fonts change
    onChange = "${pkgs.fontconfig}/bin/fc-cache -f";
  };

  # Enable fontconfig for user fonts
  fonts.fontconfig.enable = true;
}
