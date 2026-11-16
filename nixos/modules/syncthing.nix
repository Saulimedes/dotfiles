{ ... }:
{
  services.syncthing = {
    enable = true;
    user = "becker";
    dataDir = "/home/becker";
    configDir = "/home/becker/.config/syncthing";
    openDefaultPorts = true;
  };
}
