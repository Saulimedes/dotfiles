# Miscellaneous CLI tools
{ config, pkgs, lib, ... }:

{
  # Btop - system monitor
  programs.btop = {
    enable = true;
    settings = {
      # color_theme managed by Stylix
      theme_background = false;
      vim_keys = true;
      rounded_corners = true;
      update_ms = 1000;
      proc_sorting = "cpu lazy";
      proc_tree = false;
    };
  };

  # Tealdeer - tldr client
  programs.tealdeer = {
    enable = true;
    settings = {
      display = {
        compact = false;
        use_pager = false;
      };
      updates = {
        auto_update = true;
        auto_update_interval_hours = 720; # 30 days
      };
    };
  };

  # Additional packages
  home.packages = with pkgs; [
    # System info
    fastfetch
    neofetch

    # Media
    yt-dlp
    ffmpeg
    imagemagick
    mpv

    # Text editors (backup)
    nano
    micro

    # Network tools
    bandwhich
    gping
    doggo  # DNS client (replaced dogdns)

    # Process management
    procs
    bottom

    # Disk usage
    dust
    dua

    # JSON/YAML
    fx
    dasel

    # Security
    age
    sops

    # Clipboard
    wl-clipboard
    xclip

    # Document conversion
    pandoc
  ];
}
