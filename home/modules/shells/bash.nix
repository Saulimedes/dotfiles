# Bash shell configuration (secondary shell)
{ config, pkgs, lib, userConfig, ... }:

{
  programs.bash = {
    enable = true;

    historyControl = [ "ignoredups" "erasedups" ];
    historySize = 10000;
    historyFileSize = 20000;

    shellOptions = [
      "histappend"
      "checkwinsize"
      "extglob"
      "globstar"
      "checkjobs"
    ];

    shellAliases = {
      e = "$EDITOR";
      ll = "eza -la --group-directories-first --icons";
      la = "eza -a --group-directories-first";
      l = "ll";
      ".." = "cd ..";
      "..." = "cd ../..";
    };

    bashrcExtra = ''
      # Enable programmable completion
      if [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
      fi
    '';

    profileExtra = ''
      # Add local bin to PATH
      export PATH="$HOME/.local/bin:$PATH"
    '';
  };
}
