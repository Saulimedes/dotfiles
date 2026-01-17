# File management tools
{ config, pkgs, lib, ... }:

{
  # Bat - better cat
  programs.bat = {
    enable = true;
    config = {
      theme = "Nord";
      style = "numbers,changes,header";
      pager = "less -FR";
    };
  };

  # Eza - better ls
  programs.eza = {
    enable = true;
    enableFishIntegration = false; # We define our own aliases
    enableBashIntegration = false;
    icons = "auto";
    git = true;
    extraOptions = [
      "--group-directories-first"
    ];
  };

  # Ripgrep
  programs.ripgrep = {
    enable = true;
    arguments = [
      "--smart-case"
      "--follow"
      "--hidden"
      "--glob=!.git/*"
    ];
  };

  # fd - better find
  home.packages = [ pkgs.fd ];

  # Yazi file manager
  programs.yazi = {
    enable = true;
    enableFishIntegration = false; # We use custom y function
    enableBashIntegration = true;

    settings = {
      mgr = {
        ratio = [ 1 4 3 ];
        sort_by = "mtime";
        sort_sensitive = false;
        sort_reverse = true;
        sort_dir_first = true;
        linemode = "size";
        show_hidden = false;
        show_symlink = true;
        scrolloff = 5;
      };
    };
  };

  # Zoxide - smarter cd (replaces cd command)
  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
    enableBashIntegration = true;
    options = [ "--cmd cd" ];
  };
}
