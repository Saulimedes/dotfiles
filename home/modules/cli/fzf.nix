# FZF configuration
{ config, pkgs, lib, ... }:

{
  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    enableBashIntegration = true;

    defaultCommand = "rg --files --follow --no-messages";

    defaultOptions = [
      "--height 40%"
      "--layout=reverse"
      "--border"
      "--color=fg:7,bg:-1,hl:4,fg+:7,bg+:-1,hl+:4"
    ];

    fileWidgetCommand = "fd --type f --hidden --follow --exclude .git";
    fileWidgetOptions = [ "--preview 'bat --style=numbers --color=always {}'" ];

    changeDirWidgetCommand = "fd --type d --hidden --follow --exclude .git";
    changeDirWidgetOptions = [ "--preview 'eza -la --color=always {}'" ];
  };
}
