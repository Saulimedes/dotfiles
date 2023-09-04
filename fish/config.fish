# default features
set -U fish_features stderr-nocaret qmark-noglob ampersand-nobg-in-token
set fish_greeting ""
set -x DIRENV_LOG_FORMAT ""

# paths
set -e fish_user_paths
set -U fish_user_paths $fish_user_paths ~/.local/bin ~/.bin ~/.krew/bin bin ~/.config/emacs/bin ~/Applications /var/lib/flatpak/exports/bin/ ~/.cargo/bin

# exports
set -gx EDITOR "nvim"
set -gx VISUAL "emacsclient -c -a emacs" 
set -gx PAGER "bat --pager='less -FR'"
set -gx MANPAGER "sh -c 'col -bx | bat -l man -p'"

# bindings
fish_default_key_bindings
bind \b backward-kill-word
bind \e\[3\;5~ kill-word


function fish_user_key_bindings
	fzf_key_bindings
end

# configure colors
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
set -gx FZF_DEFAULT_OPTS "--height 40% --layout=reverse --color=fg:7,bg:-1,hl:4,fg+:7,bg+:-1,hl+:4"
set -x FZF_CTRL_T_OPTS "--preview 'bat {}'"
set -x FZF_ALT_C_OPTS "--preview 'exa --tree --level 1 {} | head -200'"

# helper
alias e="$EDITOR"
alias em="emacsclient -t -a ''"
alias cp="cp -airv"
alias scp="scp -r"
alias cat="bat --theme='base16-256'"
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
  set -x KUBECONFIG (string join ":" (find ~/.kube -type f \( -name "*.yml" -o -name "*.yaml" \)))
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
