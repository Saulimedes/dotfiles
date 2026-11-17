{ pkgs, ... }:
{
  services.tlp = {
    enable = true;
    settings = {
      WIFI_PWR_ON_BAT = "off";
      WIFI_PWR_ON_AC = "off";
    };
  };
  services.power-profiles-daemon.enable = false;
  services.upower.enable = true;

  services.logind.settings.Login = {
    HandleLidSwitch = "suspend";
    HandleLidSwitchExternalPower = "lock";
    HandleLidSwitchDocked = "ignore";
  };

  services.udev.packages = [ pkgs.brightnessctl ];

  environment.systemPackages = with pkgs; [
    powertop
    acpi
  ];
}
