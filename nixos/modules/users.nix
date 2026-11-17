{ pkgs, ... }:
{
  users.mutableUsers = true;

  users.users.becker = {
    isNormalUser = true;
    description = "becker";
    shell = pkgs.fish;
    extraGroups = [
      "wheel"
      "networkmanager"
      "video"
      "audio"
      "input"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHItGSiwGLCpQR+voaSj/6evQ9VXkHMtRbMVwc49Q4oq rocknix-nova-session"
    ];
  };
}
