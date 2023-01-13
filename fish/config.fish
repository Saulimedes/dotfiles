# default features
set -U fish_features stderr-nocaret qmark-noglob ampersand-nobg-in-token
set fish_greeting ""
set -x DIRENV_LOG_FORMAT ""

# paths
fish_add_path -m ~/.local/bin
fish_add_path -aP ~/.bin
fish_add_path bin/

# export
export EDITOR=nvim MANPAGER='nvim +Man!' PAGER=nvimpager USE_CCACHE=1

# hybrid keybindings
set -g fish_key_bindings fish_hybrid_key_bindings

# configure colors
set -U theme_color_scheme base16


if status --is-interactive
  # enable direnv hook
  if command -q direnv
    set -g direnv_fish_mode eval_after_arrow
    direnv-hook
    # direnv hook fish | source
  end

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
set -U FZF_PREVIEW_DIR_CMD "exa"
set -U FZF_TMUX 1
set -gx FZF_DEFAULT_COMMAND 'rg --files --follow --no-messages'
set -x FZF_DEFAULT_OPTS "--color=16"
set -x FZF_CTRL_T_OPTS "--preview 'bat {}'"
set -x FZF_ALT_C_OPTS "--preview 'exa --tree --level 1 {} | head -200'"

# helper
alias e="$EDITOR"
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
alias la="exa -a --color=always --group-directories-first"
alias ll="exa -la --color=always --group-directories-first --icons"
alias l="ll"
alias lt="exa -aT --color=always --group-directories-first"
alias "l."='exa -a | grep -E "^."'
alias lg="exa -l --group-directories-first --git-ignore --group=always"
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
  abbr gln "git clone"
  abbr gdf "git diff --name-only"
  abbr gdc "git diff --cached"
  abbr gd "git diff"
  abbr gdx "git rm -r"
  abbr gf "git fetch"
  abbr gm "git merge"
  abbr gp "git pull"
  abbr gpx "git push"
  abbr glog "git lg1"
  abbr gox "git open"
  abbr gr "git restore"
  abbr grx "git rm -r"
  abbr grb "git rebase"
  abbr gs "git s"
end

# starship prompt
if type starship &> /dev/null;
   starship init fish | source
end
