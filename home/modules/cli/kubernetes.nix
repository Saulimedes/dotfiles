# Kubernetes tools
{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    kubectl
    kubernetes-helm
    kustomize
    k9s
    kubie
    stern # Log tailing
  ];

  # K9s configuration
  programs.k9s = {
    enable = true;
    settings = {
      k9s = {
        refreshRate = 2;
        maxConnRetry = 5;
        enableMouse = true;
        headless = false;
        logoless = false;
        crumbsless = false;
        readOnly = false;
        noExitOnCtrlC = false;
        ui = {
          enableMouse = true;
          headless = false;
          logoless = false;
          crumbsless = false;
          reactive = false;
          noIcons = false;
        };
        logger = {
          tail = 100;
          buffer = 5000;
          sinceSeconds = -1;
          textWrap = false;
          showTime = false;
        };
      };
    };
  };
}
