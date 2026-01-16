# Starship prompt
{ config, pkgs, lib, ... }:

{
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    enableBashIntegration = true;

    settings = {
      add_newline = false;
      format = "$directory$git_branch$git_status$hostname$shell$character";
      command_timeout = 400;

      character = {
        success_symbol = "[>](bold green)";
        error_symbol = "[>](bold red)";
        vimcmd_symbol = "[<](bold cyan)";
      };

      directory = {
        truncation_length = 3;
        truncation_symbol = ".../";
        truncate_to_repo = true;
        substitutions = {
          "Documents" = "docs";
          "Downloads" = "dl";
          "Music" = "music";
          "Videos" = "vid";
          "Pictures" = "pic";
          "Projects" = "proj";
        };
      };

      git_branch = {
        format = "[$symbol$branch(:$remote_branch)]($style)";
        ignore_branches = [ "master" "main" ];
      };

      git_status = {
        ahead = ">[count]";
        diverged = "<>[ahead_count]>[behind_count]";
        behind = "<[count]";
        stashed = "";
      };

      hostname = {
        ssh_only = true;
        trim_at = ".";
        format = "[$ssh_symbol]($style)";
        ssh_symbol = ">";
        style = "bold green";
      };

      shell = {
        powershell_indicator = "ps";
        bash_indicator = "";
        zsh_indicator = "";
        fish_indicator = "f";
        unknown_indicator = "";
        disabled = false;
        style = "bold green";
        format = "[$indicator]($style)";
      };
    };
  };
}
