{ pkgs, ... }:
{
  services.tlp.enable = true;
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
