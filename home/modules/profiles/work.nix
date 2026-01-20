# Work profile - additional tools for work environments
{ config, pkgs, lib, hostConfig, ... }:

{
  options.profiles.work = {
    enable = lib.mkEnableOption "work profile with additional tools";
  };

  config = lib.mkIf (hostConfig.enableWork or config.profiles.work.enable) {
    home.packages = with pkgs; [
      ungoogled-chromium
    ];
  };
}
