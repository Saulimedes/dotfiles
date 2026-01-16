# Boot configuration
{ config, pkgs, lib, ... }:

{
  # Bootloader - using systemd-boot (common for UEFI systems)
  boot.loader = {
    systemd-boot = {
      enable = true;
      configurationLimit = 10;
      editor = false; # Security: disable kernel cmdline editing
    };
    efi.canTouchEfiVariables = true;
    timeout = 3;
  };

  # Kernel settings
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Enable SysRq for debugging
  boot.kernel.sysctl = {
    "kernel.sysrq" = 1;
  };

  # Plymouth boot splash (optional)
  # boot.plymouth.enable = true;

  # Tmp on tmpfs
  boot.tmp.useTmpfs = true;
  boot.tmp.tmpfsSize = "50%";
}
