# placeholder: replace with nixos-generate-config --show-hardware-config on that machine
{ lib, ... }:
{
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = true;
  boot.kernelModules = [ "kvm-amd" ];
  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "usb_storage"
    "sd_mod"
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };
}
