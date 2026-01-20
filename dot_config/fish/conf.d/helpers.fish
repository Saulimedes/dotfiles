if type -q direnv
  set -x DIRENV_LOG_FORMAT ""
  set -g direnv_fish_mode eval_after_arrow
  eval (direnv hook fish)
end

if type -q mise
  mise activate fish | source
end

if type -q zoxide
  set --universal zoxide_cmd cd
  set -Ux _ZO_EXCLUDE_DIRS "/proc/*:/sys/*:/dev/*"
  zoxide init fish | source
end

# Atuin init handled by Home Manager with --disable-up-arrow flag

if type -q devbox
  devbox global shellenv -r | source
end

if type -q fzf
  set -U FZF_DISABLE_KEYBINDINGS 1
  set -U FZF_PREVIEW_DIR_CMD "lsd"
  set -U FZF_TMUX 1
  set -gx FZF_DEFAULT_COMMAND 'rg --files --follow --no-messages'
  set -gx FZF_DEFAULT_OPTS "--height 40% --layout=reverse --color=fg:7,bg:-1,hl:4,fg+:7,bg+:-1,hl+:4"
  set -gx FZF_ALT_C_OPTS "--preview 'lsd -l --depth 1 {} | head -200'"
end

# Emacs-style keybindings
function fish_user_key_bindings
  # Start with default (emacs) bindings
  fish_default_key_bindings

  # Word navigation (emacs style)
  bind \b backward-kill-word          # Ctrl+Backspace: delete word backward
  bind \e\[3\;5~ kill-word            # Ctrl+Delete: delete word forward
  bind \ef forward-word               # Alt+F: forward word
  bind \eb backward-word              # Alt+B: backward word
  bind \ed kill-word                  # Alt+D: delete word forward

  # Line navigation (emacs style)
  bind \ca beginning-of-line          # Ctrl+A: beginning of line
  bind \ce end-of-line                # Ctrl+E: end of line
  bind \ck kill-line                  # Ctrl+K: kill to end of line
  bind \cu backward-kill-line         # Ctrl+U: kill to beginning of line

  # History
  bind \cp up-or-search               # Ctrl+P: previous history
  bind \cn down-or-search             # Ctrl+N: next history

  # FZF bindings
  if type -q fzf
    fzf_key_bindings
  end

  # Atuin: Ctrl+R for history search
  if type -q atuin
    bind \cr _atuin_search
  end
end
