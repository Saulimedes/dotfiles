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

# tmux
if status is-interactive
	if ! set -q TMUX
		exec tmux
	end
end

# configure classic prompt
set fish_color_user --bold blue
set fish_color_cwd --bold white
set fish_cursor_default     block      blink
set fish_cursor_insert      line       blink
set fish_cursor_replace_one underscore blink
set fish_cursor_visual      block

# enable colour hints in vcs prompt
set __fish_git_prompt_showcolorhints yes
set __fish_git_prompt_color_prefix purple
set __fish_git_prompt_color_suffix purple

# enable direnv hook
if type direnv &> /dev/null
    eval (direnv hook fish)
end

# fzf
setenv FZF_DEFAULT_COMMAND 'fd --type file --follow'
setenv FZF_ALT_C_COMMAND "$FZF_CTRL_T_COMMAND"
setenv FZF_CTRL_T_COMMAND "fd -t d --hidden --follow --exclude \".git\" . $HOME"
setenv FZF_DEFAULT_OPTS '--height 20%'

# zoxide
set --universal zoxide_cmd cd

# ssh
if test -z (pgrep ssh-agent | string collect)
  eval (ssh-agent -c)
  set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK
  set -Ux SSH_AGENT_PID $SSH_AGENT_PID
end

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
  alias kubectx="kctx"
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
  alias kt="stern"
  alias kwatch="kubectl get po --all-namespaces"
  abbr kg "kubectl get"
  abbr kd "kubectl describe"
  abbr kgs "kubectl get service"
  abbr kgp "kubectl get pods -l"
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
  abbr go "git open"
  abbr gr "git restore"
  abbr grx "git rm -r"
  abbr grb "git rebase"
  abbr gs "git s"
end

# starship prompt
if type starship &> /dev/null;
   starship init fish | source
end
