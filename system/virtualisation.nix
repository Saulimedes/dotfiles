# Virtualisation and containers
{ config, pkgs, lib, userConfig, ... }:

{
  # Docker
  virtualisation.docker = {
    enable = true;
    autoPrune = {
      enable = true;
      dates = "weekly";
    };
  };

  # Podman (alternative to Docker)
  virtualisation.podman = {
    enable = true;
    dockerCompat = false; # Set to true if you want podman to act as docker
    defaultNetwork.settings.dns_enabled = true;
  };

  # Libvirt for VMs
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = true;
      swtpm.enable = true;
      ovmf = {
        enable = true;
        packages = [ pkgs.OVMFFull.fd ];
      };
    };
  };

  # Virt-manager GUI
  programs.virt-manager.enable = true;

  # USB passthrough support
  virtualisation.spiceUSBRedirection.enable = true;
}
