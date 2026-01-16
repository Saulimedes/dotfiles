# User configuration
{ config, pkgs, lib, userConfig, ... }:

{
  # User account
  users.users.${userConfig.username} = {
    isNormalUser = true;
    description = userConfig.fullName;
    extraGroups = [
      "wheel" # sudo access
      "networkmanager"
      "video"
      "audio"
      "docker"
      "podman"
      "libvirtd"
    ];
    shell = pkgs.fish;
  };

  # Enable fish system-wide for proper integration
  programs.fish.enable = true;

  # Sudo configuration
  security.sudo = {
    enable = true;
    wheelNeedsPassword = true;
    extraRules = [
      {
        users = [ userConfig.username ];
        commands = [
          {
            command = "/run/current-system/sw/bin/nixos-rebuild";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];
  };
}
