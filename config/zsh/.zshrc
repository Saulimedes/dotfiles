# Load Antidote plugin manager
ZDOTDIR=$HOME/.zsh
ABBR_USER_ABBREVIATIONS_FILE=$ZDOTDIR/abbreviations
ABBR_SET_EXPANSION_CURSOR=1
zsh_plugins=${ZDOTDIR}/zsh_plugins
source $ZDOTDIR/antidote/antidote.zsh
antidote load

# HISTORY
setopt APPEND_HISTORY SHARE_HISTORY HIST_IGNORE_ALL_DUPS HIST_REDUCE_BLANKS
HISTSIZE=10000
SAVEHIST=50000
HISTFILE=$HOME/.local/share/zsh_history

# Initialize Zsh completion system
autoload -Uz compinit
compinit
setopt menucomplete autolist automenu complete_in_word
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=blue'

# Zstyle configurations
zstyle ':completion:*' menu select=1            # Show menu after the first ambiguous completion
zstyle ':completion:*' insert-unambiguous yes
zstyle ':completion:*:default' list-prompt '%Sscroll: %p'  # Customize the list prompt
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' accept-exact 'yes'

# Enable completion caching
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*' cache-path ~/.cache/zsh/completions

# Group and format completion output
zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*:messages' format '%F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format '%F{red}-- no matches found --%f'
zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

zstyle ':completion:*:*:git:*' use-cache on
zstyle ':completion:*:*:git:*' menu select=1
zstyle ':completion:*:*:git:*' accept-exact true
zstyle ':completion:*:*:git:*' insert-unambiguous true

# Improve the selection process
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

## custom
zstyle ':completion:*:*:git:*' user-commands brancher:'switch or create git branch'

# Disable problematic features
setopt NO_BEEP NO_BG_NICE

# Enable typo correction
setopt CORRECT
setopt CORRECT_ALL

# Enable completion of wildcard patterns for all file types
zstyle ':completion:*' file-patterns '*:all-files'
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' completer _expand _complete _correct _ignored _approximate


# Enable globbing
setopt GLOB_COMPLETE

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
bindkey '^I' expand-or-complete
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
  eval $(ssh-agent -s) > /dev/null
  export SSH_AUTH_SOCK
  export SSH_AGENT_PID
fi

# Aliases
alias e="$EDITOR"
alias cp="cp -airv"
alias scp="scp -r"
alias cat="bat"
alias dd="sudo dd status=progress bs=4M oflag=sync"
alias fdisk="sudo fdisk -l"
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
alias back="cd $OLDPWD"
alias docker="podman"

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

# functions
for file in ~/.zsh/functions/*.zsh; do
  [ -r "$file" ] && source "$file"
done

# Starship prompt
if type starship &>/dev/null; then
  eval "$(starship init zsh)"
fi

