# Starship prompt
{ config, pkgs, lib, ... }:

{
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    enableBashIntegration = true;

    settings = {
      add_newline = false;
      format = "$directory$git_branch$git_status$hostname$character";
      right_format = "$cmd_duration$time";
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
        format = "[$symbol$branch(:$remote_branch)]($style) ";
        ignore_branches = [ "master" "main" ];
      };

      git_status = {
        ahead = ">$count";
        diverged = "<>$ahead_count>$behind_count";
        behind = "<$count";
        stashed = "";
      };

      hostname = {
        ssh_only = true;
        trim_at = ".";
        format = "[$ssh_symbol]($style)";
        ssh_symbol = ">";
        style = "bold green";
      };

      # Command duration (shows on right when > 2s)
      cmd_duration = {
        min_time = 2000;
        format = "[$duration]($style) ";
        style = "yellow";
      };

      # Time module (disabled by default, toggle with 'prompt_time')
      time = {
        disabled = true;
        format = "[$time]($style)";
        style = "dimmed white";
        time_format = "%H:%M";
      };
    };
  };

  # Alternative config with time enabled
  xdg.configFile."starship-time.toml".text = ''
    add_newline = false
    format = "$directory$git_branch$git_status$hostname$character"
    right_format = "$cmd_duration$time"
    command_timeout = 400

    [character]
    success_symbol = "[>](bold green)"
    error_symbol = "[>](bold red)"
    vimcmd_symbol = "[<](bold cyan)"

    [directory]
    truncation_length = 3
    truncation_symbol = ".../"
    truncate_to_repo = true

    [directory.substitutions]
    Documents = "docs"
    Downloads = "dl"
    Music = "music"
    Videos = "vid"
    Pictures = "pic"
    Projects = "proj"

    [git_branch]
    format = "[$symbol$branch(:$remote_branch)]($style) "
    ignore_branches = ["master", "main"]

    [git_status]
    ahead = ">$count"
    diverged = "<>$ahead_count>$behind_count"
    behind = "<$count"
    stashed = ""

    [hostname]
    ssh_only = true
    trim_at = "."
    format = "[$ssh_symbol]($style)"
    ssh_symbol = ">"
    style = "bold green"

    [cmd_duration]
    min_time = 2000
    format = "[$duration]($style) "
    style = "yellow"

    [time]
    disabled = false
    format = "[$time]($style)"
    style = "dimmed white"
    time_format = "%H:%M"
  '';
}
