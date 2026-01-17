# Virtualisation and containers
{ config, pkgs, lib, userConfig, ... }:

{
  # Podman (rootless containers)
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;  # Provides 'docker' command alias
    dockerSocket.enable = true;  # Provides /var/run/docker.sock compatibility
    defaultNetwork.settings.dns_enabled = true;
    autoPrune = {
      enable = true;
      dates = "weekly";
    };
  };

  # Libvirt for VMs
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = true;
      swtpm.enable = true;
      # OVMF is now included by default
    };
  };

  # Virt-manager GUI
  programs.virt-manager.enable = true;

  # USB passthrough support
  virtualisation.spiceUSBRedirection.enable = true;
}
