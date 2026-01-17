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
      filter_mode_shell_up_key_binding = "session";  # Up arrow: session only, deduped
      search_mode_shell_up_key_binding = "prefix";   # Up arrow: prefix match
      style = "compact";
      auto_sync = true;
      sync_frequency = "6h";
      secrets_filter = true;
      store_failed = false;  # Don't store failed commands
      show_preview = true;
      show_help = false;
      inline_height = 20;
      word_jump_mode = "emacs";
      history_format = "plain";

      ignored_commands = [
        "cd"
        "cdi"
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
