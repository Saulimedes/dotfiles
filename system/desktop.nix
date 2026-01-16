# Desktop environment configuration
{ config, pkgs, lib, ... }:

{
  # X11 / Wayland display server
  services.xserver = {
    enable = true;
    xkb = {
      layout = "us";
      variant = "";
    };
  };

  # LightDM + XFCE Desktop Environment
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.xfce.enable = true;

  # Alternative: GNOME
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;

  # Alternative: KDE Plasma
  # services.displayManager.sddm.enable = true;
  # services.desktopManager.plasma6.enable = true;

  # Alternative: Hyprland (Wayland compositor)
  # programs.hyprland.enable = true;

  # XDG Portal for screen sharing, file picking, etc.
  xdg.portal = {
    enable = true;
    # extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # Fonts
  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      noto-fonts
      noto-fonts-color-emoji
      liberation_ttf
      fira-code
      fira-code-symbols
      nerd-fonts.jetbrains-mono
      nerd-fonts.iosevka
      nerd-fonts.iosevka-term
      inter
    ];
    fontconfig = {
      defaultFonts = {
        serif = [ "Noto Serif" ];
        sansSerif = [ "Inter" "Noto Sans" ];
        monospace = [ "JetBrainsMono Nerd Font" ];
        emoji = [ "Noto Color Emoji" ];
      };
    };
  };

  # Flatpak support
  services.flatpak.enable = true;

  # GNOME keyring for credential storage
  services.gnome.gnome-keyring.enable = true;

  # Thunar file manager plugins (for XFCE)
  programs.thunar = {
    enable = true;
    plugins = with pkgs; [ thunar-archive-plugin thunar-volman ];
  };
}
