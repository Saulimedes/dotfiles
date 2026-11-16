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
  };
}
