# default features
set -U fish_features stderr-nocaret qmark-noglob ampersand-nobg-in-token
set fish_greeting ""
set -x DIRENV_LOG_FORMAT ""

# paths
set -gx PATH /usr/local/bin /usr/bin /usr/sbin /usr/local/sbin $PATH
set -U fish_user_paths $fish_user_paths ~/.local/bin ~/.bin ~/.krew/bin bin ~/.config/emacs/bin ~/Applications /var/lib/flatpak/exports/bin/ ~/.cargo/bin 

# exports
set -gx EDITOR "nvim"
set -gx VISUAL "emacsclient -c -a emacs" 

# bindings
fish_default_key_bindings

function fish_user_key_bindings
  bind \b backward-kill-word
  bind \e\[3\;5~ kill-word
 
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

if status --is-interactive
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

  # ssh
  if test -z (pgrep ssh-agent | string collect)
    eval (ssh-agent -c)
    set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK
    set -Ux SSH_AGENT_PID $SSH_AGENT_PID
  end
end

# fzf
set -U FZF_DISABLE_KEYBINDINGS 1
set -U FZF_PREVIEW_DIR_CMD "lsd"
set -U FZF_TMUX 1
set -gx FZF_DEFAULT_COMMAND 'rg --files --follow --no-messages'
set -gx FZF_DEFAULT_OPTS "--height 40% --layout=reverse --color=fg:7,bg:-1,hl:4,fg+:7,bg+:-1,hl+:4"

#set -x FZF_CTRL_T_OPTS "--preview 'bat {}'"
set -x FZF_ALT_C_OPTS "--preview 'lsd -l --depth 1 {} | head -200'"

# helper
alias e="$EDITOR"
alias em="emacsclient -t -a ''"
alias cp="cp -airv"
alias scp="scp -r"
alias cat="bat"
alias cls="clear; printf '\033[3J'"
alias dd="dd status=progress"
alias mkdir="mkdir -p"
alias privatemode="fish --private"
alias ip="ip --color"
alias diff="diff --color=auto"
alias vdir="vdir --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias ls="ls --color=auto"
alias la="lsd -a --group-dirs first"
alias ll="lsd -la --group-dirs first --icon always"
alias l="ll"
alias "l."='lsd -a | grep -E "^."'
alias lg="lsd -l --group-dirs first --ignore-glob .git --icon always"
alias lt="lsd -t --tree --group-dirs first --depth 4"
alias mv="mv -iv"
alias sudoe="sudo -E"
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

## kubectl
if type -q kubectl
  if test -d ~/.kube
    set -x KUBECONFIG (string join ":" (fd --max-depth 1 --type f . ~/.kube | xargs -I {} sh -c 'grep -qm1 "apiVersion:\|kind:\|clusters:\|contexts:\|users:" "{}" && echo "{}"'))
  end

  alias k="kubectl"
  alias kgd="kubectl get deployments -o wide"
  alias kg="kubectl get"
  alias kgh="kubectl get hr -o wide"
  alias kge="kubectl get events --watch"
  alias kgsv="kubectl get service -o wide"
  alias krr="kubectl rollout restart"
  alias krs="kubectl rollout status"
  alias kgy="kubectl get -o yaml"
  alias kaf="kubectl apply -f"
  alias krm="kubectl delete"
  alias kwatch="kubectl get po --all-namespaces"
  abbr kg "kubectl get"
  abbr kd "kubectl describe"
  abbr kgs "kubectl get service"
  abbr kgp "kubectl get pods"
  abbr kex "kubectl exec -it"
  abbr kl "kubectl logs -f -p"
  abbr kdel "kubectl del"
  abbr kgwf "watch kubectl get -f"
end

if type -q git
  abbr g "git"
  abbr ga "git add"
  abbr gb "git brancher"
  abbr gc "git commit -m"
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

# starship prompt
if type starship &> /dev/null;
   starship init fish | source
end
