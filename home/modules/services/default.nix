# User services configuration
{ config, pkgs, lib, ... }:

{
  # Syncthing file synchronization
  services.syncthing = {
    enable = true;
    # tray.enable = true; # Uncomment for tray icon
  };

  # Custom systemd user services
  systemd.user.services = {
    # Podman cleanup service
    podman-cleanup = {
      Unit = {
        Description = "Podman prune job";
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.podman}/bin/podman system prune -af && ${pkgs.podman}/bin/podman volume prune -f'";
      };
    };
  };

  # Systemd timers
  systemd.user.timers = {
    podman-cleanup = {
      Unit = {
        Description = "Run podman cleanup weekly";
      };
      Timer = {
        OnCalendar = "weekly";
        Persistent = true;
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };

  # GPG agent for Git signing and SSH support
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;  # Handles SSH keys - no need for fish-ssh-agent
    pinentry.package = pkgs.pinentry-emacs;  # Integrates with Emacs, falls back to curses
    defaultCacheTtl = 3600;
    maxCacheTtl = 86400;
  };
}
