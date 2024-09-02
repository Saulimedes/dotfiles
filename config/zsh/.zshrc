# Load Antidote plugin manager
ZDOTDIR=$HOME/.zsh
zsh_plugins=${ZDOTDIR}/zsh_plugins
source $ZDOTDIR/antidote/antidote.zsh
antidote load

# Initialize Zsh completion system
autoload -Uz compinit
compinit
setopt menucomplete autolist automenu

# Zstyle configurations
zstyle ':completion:*' menu select=1            # Show menu after the first ambiguous completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'  # Case-insensitive completion
zstyle ':completion:*:default' list-prompt '%Sscroll: %p'  # Customize the list prompt

# Group and format completion output
zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*:messages' format '%F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format '%F{red}-- no matches found --%f'

# Improve the selection process
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

# Disable problematic features
setopt NO_BEEP NO_BG_NICE

# Environment and PATH configuration
export PATH="/usr/local/bin:/usr/bin:/usr/sbin:/usr/local/sbin:$PATH"
typeset -U path
path=($HOME/.local/bin $HOME/.bin $HOME/.krew/bin $HOME/bin $HOME/Applications /var/lib/flatpak/exports/bin $HOME/.cargo/bin $path)

# Exports
export EDITOR="nvim"
export VISUAL="emacsclient -c -a emacs"
export MANPAGER='nvim +Man!'
export PYENV_ROOT="$HOME/.local/share/pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

## zoxide
eval "$(zoxide init --cmd cd zsh)"

# Key bindings
bindkey "^H" backward-delete-char
bindkey "^[[3;5~" kill-word
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# Direnv configuration
if type direnv &>/dev/null; then
  eval "$(direnv hook zsh)"
fi

# FZF configuration
export FZF_DEFAULT_COMMAND='rg --files --follow --no-messages'
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --color=fg:7,bg:-1,hl:4,fg+:7,bg+:-1,hl+:4"
export FZF_ALT_C_OPTS="--preview 'lsd -l --depth 1 {} | head -200'"

# SSH agent setup
if [[ -z $(pgrep ssh-agent) ]]; then
  eval $(ssh-agent -c)
  export SSH_AUTH_SOCK
  export SSH_AGENT_PID
fi

# Aliases
alias e="$EDITOR"
alias cp="cp -airv"
alias scp="scp -r"
alias cat="bat"
alias dd="dd status=progress"
alias mkdir="mkdir -p"
alias nohist="zsh --no-history"
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

# kubernetes
if command -v kubectl >/dev/null 2>&1; then
  if [ -d "$HOME/.kube" ]; then
    export KUBECONFIG=$(find "$HOME/.kube" -maxdepth 1 -type f | while IFS= read -r file; do
        if grep -qm1 -e "apiVersion:" -e "kind:" -e "clusters:" -e "contexts:" -e "users:" "$file"; then
            echo -n "$file:"
        fi
    done | sed 's/:$//')
  fi
fi

# Starship prompt
if type starship &>/dev/null; then
  eval "$(starship init zsh)"
fi
