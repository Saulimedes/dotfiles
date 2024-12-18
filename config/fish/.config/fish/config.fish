# default features
set -U fish_features stderr-nocaret qmark-noglob ampersand-nobg-in-token
set fish_greeting ""
set -x DIRENV_LOG_FORMAT ""

# paths
set -gx PATH /usr/local/bin /usr/bin /usr/sbin /usr/local/sbin $PATH
set -U fish_user_paths $fish_user_paths ~/.local/bin ~/.bin ~/.krew/bin bin ~/Applications /var/lib/flatpak/exports/bin/ ~/.cargo/bin 

## fish plugins
set fisher_path $__fish_config_dir/plugins
set fish_complete_path $fish_complete_path[1] $fisher_path/completions $fish_complete_path[2..]
set fish_function_path $fish_function_path[1] $fisher_path/functions $fish_function_path[2..]

for file in $fisher_path/conf.d/*.fish
    source $file
end

# exports
set -gx EDITOR "nvim"
set -gx VISUAL "emacsclient -c -a emacs" 
set -Ux MANPAGER 'nvim +Man!'

bind \cp _fish_ai_codify_or_explain
bind -k nul _fish_ai_autocomplete_or_fix

if status --is-interactive
  # bindings
  fish_default_key_bindings

  function fish_user_key_bindings
    bind \b backward-kill-word
    bind \e\[3\;5~ kill-word
    bind \cp up-or-search
    bind \cx\cf _fish_ai_codify_or_explain

  
    # fzf bindings
    fzf_key_bindings
    if type -q fzf-history-widget
      function __custom_fzf_history
        history --merge
        fzf-history-widget
      end

      bind \cr __custom_fzf_history
    end  
  end

  # direnv hook
  if command -q direnv
    set -g direnv_fish_mode eval_after_arrow
    direnv-hook
  end

  # pyenv
  set -x PYENV_ROOT $HOME/.local/share/pyenv
  set -x PATH $PYENV_ROOT/bin $PATH
  source (pyenv init - | psub)

  # zoxide
  set --universal zoxide_cmd cd

  # fzf
  set -U FZF_DISABLE_KEYBINDINGS 1
  set -U FZF_PREVIEW_DIR_CMD "lsd"
  set -U FZF_TMUX 1
  set -gx FZF_DEFAULT_COMMAND 'rg --files --follow --no-messages'
  set -gx FZF_DEFAULT_OPTS "--height 40% --layout=reverse --color=fg:7,bg:-1,hl:4,fg+:7,bg+:-1,hl+:4"
  set -x FZF_ALT_C_OPTS "--preview 'lsd -l --depth 1 {} | head -200'"

  # ssh
  if test -z (pgrep ssh-agent | string collect)
    eval (ssh-agent -c)
    set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK
    set -Ux SSH_AGENT_PID $SSH_AGENT_PID
  end

  # starship prompt
  if type starship &> /dev/null;
    starship init fish | source
  end
end

# helper
alias e="$EDITOR"
alias cp="cp -airv"
alias scp="scp -r"
alias cat="bat"
alias dd="dd status=progress"
alias mkdir="mkdir -p"
alias nohist="fish --private"
alias ip="ip --color"
alias diff="diff --color=auto"
alias vdir="vdir --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias ls="eza"
alias la="eza -a --group-directories-first"
alias ll="eza -la --group-directories-first --icons "
alias l="ll"
alias l.='eza -a  | grep -E "^\\."'
alias lg="eza -l --group-directories-first --ignore-glob='.git' --icons"
alias lt="eza -T --group-directories-first --level=4"
alias mv="mv -iv"
alias rm="rm -rf"
alias ln="ln -vi"
alias rg="rg --color always"
alias getip="curl ifconfig.me"
alias ".."="cd .."
alias "..."="cd ../.."
alias ".3"="cd ../../.."
alias "...."=".3"
alias ".4"="cd ../../../.."
alias "....."=".4"
alias ".5"="cd ../../../../.."
alias "......"=".5"
abbr s "sudo"
abbr se "sudo edit"
abbr sea "sudo zypper search"
abbr i "sudo zypper in"
abbr u "sudo zypper update"
abbr sy "sudo systemctl"

## kubectl
if type -q kubectl
  if test -d ~/.kube
    set -x KUBECONFIG (string join ":" (fd --max-depth 1 --type f . ~/.kube | xargs -I {} sh -c 'grep -qm1 "apiVersion:\|kind:\|clusters:\|contexts:\|users:" "{}" && echo "{}"'))
  end

  abbr k "kubectl"
  abbr kgd "kubectl get deployments -o wide"
  abbr kg "kubectl get"
  abbr kgp "kubectl get pods"
  abbr kgh "kubectl get hr -o wide"
  abbr kge "kubectl get events --watch"
  abbr kgw --set-cursor=! 'kubectl get pod ! --watch'
  abbr kgsv "kubectl get service -o wide"
  abbr ka "kubectl apply -f"
  abbr kde "kubectl delete"
  alias kwatch="kubectl get po --all-namespaces"
  abbr kd "kubectl describe"
  abbr kgs "kubectl get service"
  abbr kex --set-cursor=! 'kubectl exec -i ! /bin/bash'
  abbr kl "kubectl logs -f -p"
  abbr kw "watch kubectl get -f"
end

if type -q git
  abbr g "git"
  abbr ga "git add"
  abbr gb "git brancher"
  abbr gc --set-cursor=! 'git commit -m "!"'
  abbr gdf "git diff --name-only"
  abbr gdc "git diff --cached"
  abbr gd "git diff"
  abbr gdx "git rm -r"
  abbr gf "git fetch"
  abbr gm "git merge"
  abbr gp "git pull"
  abbr gpx "git push"
  abbr gl "git lg1"
  abbr gox "git open"
  abbr gr "git restore"
  abbr grx "git rm -r"
  abbr grb "git rebase"
  abbr gs "git s"
  alias newtag='git tag -a'
end

# old-style highlighting
set fish_color_normal normal
set fish_color_command --bold
set fish_color_param cyan
set fish_color_redirection brblue
set fish_color_comment red
set fish_color_error brred
set fish_color_escape bryellow --bold
set fish_color_operator bryellow
set fish_color_end brmagenta
set fish_color_quote yellow
set fish_color_autosuggestion 555 brblack
set fish_color_user brgreen
