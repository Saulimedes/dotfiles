# Font configuration
{ config, pkgs, lib, ... }:

{
  # Fonts from nixpkgs
  home.packages = with pkgs; [
    # Nerd Fonts
    (nerdfonts.override {
      fonts = [
        "JetBrainsMono"
        "Iosevka"
        "IosevkaTerm"
        "FiraCode"
        "Hack"
      ];
    })

    # Sans-serif
    inter
    roboto
    source-sans
    ubuntu_font_family

    # Serif
    source-serif
    libertinus

    # Monospace
    jetbrains-mono
    fira-code
    source-code-pro

    # Icons and emoji
    font-awesome
    noto-fonts-emoji
    material-design-icons

    # CJK (if needed)
    # noto-fonts-cjk-sans

    # Font tools
    fontconfig
  ];

  # Private fonts from git submodule
  # Uncomment after adding your fonts repo as submodule:
  #   git submodule add <your-fonts-repo-url> fonts
  # home.file.".local/share/fonts/private" = {
  #   source = ../../fonts;
  #   recursive = true;
  # };

  # Enable fontconfig for user fonts
  fonts.fontconfig.enable = true;
}
