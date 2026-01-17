# Fish shell configuration (Primary shell)
{ config, pkgs, lib, ... }:

{
  programs.fish = {
    enable = true;

    # Fish plugins
    plugins = [
      {
        name = "bass";
        src = pkgs.fishPlugins.bass.src;
      }
      {
        name = "autopair";
        src = pkgs.fishPlugins.autopair.src;
      }
      {
        name = "done";
        src = pkgs.fishPlugins.done.src;
      }
      {
        name = "fzf-fish";
        src = pkgs.fishPlugins.fzf-fish.src;
      }
      # kubectl completions
      {
        name = "fish-kubectl-completions";
        src = pkgs.fetchFromGitHub {
          owner = "evanlucas";
          repo = "fish-kubectl-completions";
          rev = "ced676392575d618d8b80b3895cdc3159be3f628";
          sha256 = "sha256-OYiYTW+g71vD9NWOcX1i2/TaQfAg+c2dJZ5ohwWSDCc=";
        };
      }
      # fish-ssh-agent and vfish - removed, repos unavailable
    ];

    # Shell initialization
    interactiveShellInit = ''
      # Disable greeting
      set fish_greeting ""

      # Enable vi mode (replaces vfish plugin)
      fish_vi_key_bindings

      # Restore Ctrl+A/Ctrl+E in vi insert mode
      bind -M insert \ca beginning-of-line
      bind -M insert \ce end-of-line

      # Fish features
      set -U fish_features stderr-nocaret qmark-noglob ampersand-nobg-in-token

      # Kubectl config aggregation
      if type -q kubectl
        if test -d ~/.kube
          set -l configs (find ~/.kube -maxdepth 1 -type f -name '*.yaml' -o -name '*.yml' -o -name 'config' 2>/dev/null | tr '\n' ':' | sed 's/:$//')
          if test -n "$configs"
            set -gx KUBECONFIG $configs
          end
        end
      end
    '';

    # Shell aliases
    shellAliases = {
      # Editor
      e = "$EDITOR";

      # File operations
      cp = "cp -airv";
      scp = "scp -r";
      cat = "bat";
      dd = "dd status=progress";
      mkdir = "mkdir -p";
      mv = "mv -iv";
      rm = "rm -rf";
      ln = "ln -vi";

      # Display
      ip = "ip --color";
      diff = "diff --color=auto";
      rg = "rg --color always";

      # Eza (ls replacement)
      la = "eza -a --group-directories-first";
      ll = "eza -la --group-directories-first --icons";
      l = "ll";
      "l." = "eza -a | grep -E '^\\.'";
      lg = "eza -l --group-directories-first --ignore-glob='.git' --icons";
      lt = "eza -T --group-directories-first --level=4";

      # Navigation
      ".." = "cd ..";
      "..." = "cd ../..";
      ".3" = "cd ../../..";
      ".4" = "cd ../../../..";
      ".5" = "cd ../../../../..";

      # Utilities
      nohist = "fish --private";
      getip = "curl ifconfig.me";
      ff = "fastfetch";

      # Kubernetes
      kwatch = "kubectl get po --all-namespaces";
      newtag = "git tag -a";
    };

    # Fish abbreviations (expand on space)
    shellAbbrs = {
      # System
      s = "sudo";
      se = "sudo edit";
      sy = "sudo systemctl";

      # Git
      g = "git";
      ga = "git add";
      gb = "git brancher";
      gc = "git commit -m";
      gdf = "git diff --name-only";
      gdc = "git diff --cached";
      gd = "git diff";
      gdx = "git rm -r";
      gf = "git fetch";
      gm = "git merge";
      gp = "git pull";
      gpx = "git push";
      gl = "git lg1";
      gox = "git open";
      gr = "git restore";
      grx = "git rm -r";
      grb = "git rebase";
      gs = "git s";

      # Kubernetes
      k = "kubectl";
      kgd = "kubectl get deployments -o wide";
      kg = "kubectl get";
      kgp = "kubectl get pods -o wide";
      kgh = "kubectl get hr -o wide";
      kge = "kubectl get events --watch";
      kgsv = "kubectl get service -o wide";
      ka = "kubectl apply -f";
      kde = "kubectl delete";
      kd = "kubectl describe";
      kgs = "kubectl get service";
      kl = "kubectl logs -f -p";
      kw = "watch kubectl get -f";
    };

    # Custom functions
    functions = {
      # Yazi file manager with directory change
      y = ''
        set tmp (mktemp -t "yazi-cwd.XXXXXX")
        yazi $argv --cwd-file="$tmp"
        if read -z cwd < "$tmp"; and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
          builtin cd -- "$cwd"
        end
        rm -f -- "$tmp"
      '';

      # Make directory and cd into it
      mcd = ''
        mkdir -p $argv[1]
        cd $argv[1]
      '';

      # Posix compatibility wrapper
      posix = "bash -c $argv";

      # Command not found handler
      fish_command_not_found = ''
        echo "fish: Unknown command: $argv[1]" >&2
      '';

      # Toggle starship time display
      prompt_time = ''
        if set -q STARSHIP_CONFIG
          set -e STARSHIP_CONFIG
          echo "Time display: OFF"
        else
          set -gx STARSHIP_CONFIG ~/.config/starship-time.toml
          echo "Time display: ON"
        end
      '';
    };
  };
}
