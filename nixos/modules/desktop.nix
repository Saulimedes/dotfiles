{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ./emacs.nix ];

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  services.greetd = {
    enable = true;
    useTextGreeter = true;
    settings.default_session = {
      user = "greeter";
      command = lib.concatStringsSep " " [
        (lib.getExe pkgs.tuigreet)
        "--time"
        "--asterisks"
        "--remember"
        "--remember-session"
        "--cmd ${lib.getExe' config.programs.hyprland.package "start-hyprland"}"
        "--sessions ${config.services.displayManager.sessionData.desktops}/share/wayland-sessions"
      ];
    };
  };

  # hyprlock cannot unlock without a PAM service
  security.pam.services.hyprlock = { };

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };
  services.blueman.enable = true;

  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  services.flatpak.enable = true;

  xdg.mime.defaultApplications = {
    "text/plain" = "emacsclient.desktop";
    "text/markdown" = "emacsclient.desktop";
    "application/json" = "emacsclient.desktop";
    "application/x-shellscript" = "emacsclient.desktop";
    "x-scheme-handler/http" = "brave-origin.desktop";
    "x-scheme-handler/https" = "brave-origin.desktop";
    "text/html" = "brave-origin.desktop";
    "inode/directory" = "thunar.desktop";
  };

  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland;xcb";
    SDL_VIDEODRIVER = "wayland";
  };

  programs.nm-applet = {
    enable = true;
    indicator = true;
  };

  systemd.user.services.hyprpolkitagent = {
    description = "Hyprland polkit authentication agent";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.hyprpolkitagent}/libexec/hyprpolkitagent";
      Restart = "on-failure";
      RestartSec = 2;
    };
  };

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-color-emoji
      liberation_ttf
      fira-code
      jetbrains-mono
      nerd-fonts.jetbrains-mono
      nerd-fonts.symbols-only
    ];
    # BerkeleyMono comes from ~/.local/share/fonts via .chezmoiexternal.toml
    fontconfig.defaultFonts = {
      monospace = [
        "BerkeleyMono Nerd Font"
        "JetBrainsMono Nerd Font"
      ];
      sansSerif = [ "Noto Sans" ];
      serif = [ "Noto Serif" ];
      emoji = [ "Noto Color Emoji" ];
    };
  };

  environment.systemPackages = with pkgs; [
    hyprlock
    hypridle
    waybar
    mako
    wl-clipboard
    wl-clip-persist
    grim
    slurp
    brightnessctl
    playerctl
    wev
    glib
    libnotify
    pavucontrol
    wiremix
    networkmanagerapplet
    pa_applet

    thunar
    thunar-archive-plugin
    thunar-volman
    xfce4-settings
    tumbler

    kitty
    brave-origin
    telegram-desktop
    session-desktop
  ];
}
