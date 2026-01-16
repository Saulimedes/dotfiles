# Atuin - shell history
{ config, pkgs, lib, ... }:

{
  programs.atuin = {
    enable = true;
    enableFishIntegration = true;
    enableBashIntegration = true;

    settings = {
      search_mode = "skim";
      filter_mode = "global";
      style = "compact";
      auto_sync = true;
      sync_frequency = "6h";
      secrets_filter = true;
      show_preview = true;
      show_help = false;
      inline_height = 20;
      word_jump_mode = "emacs";
      history_format = "plain";

      ignored_commands = [
        "cd"
        "zoxide"
        "ls"
        "eza"
        "nvim"
        "ydl"
        "ll"
        "la"
        "sync"
        "exit"
        "clear"
      ];
    };
  };
}
