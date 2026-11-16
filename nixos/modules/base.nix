{
  lib,
  pkgs,
  inputs,
  hostname,
  ...
}:
{
  system.stateVersion = "26.05";
  networking.hostName = hostname;

  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      auto-optimise-store = true;
      trusted-users = [
        "root"
        "@wheel"
      ];
      warn-dirty = false;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 21d";
    };
    registry.nixpkgs.flake = inputs.nixpkgs;
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
  };
  nixpkgs.config.allowUnfree = true;

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 20;
        editor = false;
      };
      efi.canTouchEfiVariables = true;
    };
    initrd.systemd.enable = true;
    kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;
  };
  hardware.enableRedistributableFirmware = true;

  services.xserver.xkb = {
    layout = "us";
    variant = "altgr-weur";
    options = "ctrl:nocaps";
  };
  console = {
    useXkbConfig = true;
    earlySetup = true;
  };

  time.timeZone = "Europe/Berlin";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "de_DE.UTF-8";
      LC_PAPER = "de_DE.UTF-8";
      LC_MEASUREMENT = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
    };
  };

  # vpnc, sstp and fortisslvpn plugins are flagged insecure in nixpkgs
  networking.networkmanager = {
    enable = true;
    plugins = with pkgs; [
      networkmanager-openvpn
      networkmanager-openconnect
      networkmanager-l2tp
    ];
  };

  services.envfs.enable = true;
  programs.nix-ld.enable = true;

  programs.zsh = {
    enable = true;
    promptInit = "";
    enableGlobalCompInit = false;
  };
  programs.fish.enable = true;
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  security.polkit.enable = true;

  services.fstrim.enable = true;
  services.fwupd.enable = true;
  services.udisks2.enable = true;
  services.gvfs.enable = true;
  zramSwap.enable = true;

  environment.systemPackages = with pkgs; [
    zoxide
    atuin
    starship
    fzf
    bat
    eza
    fd
    ripgrep
    tmux
    neovim
    jq
    yq-go
    tree
    tealdeer
    mise

    btop
    fastfetch
    chafa
    ncdu
    duf
    lsof
    strace
    pciutils
    usbutils

    btrfs-progs
    dosfstools
    e2fsprogs
    exfatprogs
    f2fs-tools
    ntfs3g
    squashfsTools
    xfsprogs
    cryptsetup

    zip
    unzip
    p7zip
    unrar

    git
    git-lfs
    git-crypt
    gnupg
    gopass
    pinentry-emacs
    pinentry-gnome3

    curl
    wget
    rsync
    bind
    nmap
    mtr
    tcpdump
    socat
    netcat-openbsd
    whois
    iperf
    inetutils

    mpv
    ffmpeg
    libva-utils
    ffmpegthumbnailer
    mediainfo
    vips
    poppler-utils

    hunspell
    hunspellDicts.en_US
    hunspellDicts.de_DE

    android-tools
    xdg-user-dirs
    xdg-utils
    nixfmt
    nil
  ];
}
