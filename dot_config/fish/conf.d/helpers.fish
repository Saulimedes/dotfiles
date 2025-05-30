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

if type -q atuin
  atuin init fish --disable-up-arrow | source
end

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

  fish_default_key_bindings
  function fish_user_key_bindings
    bind \b backward-kill-word
    bind \e\[3\;5~ kill-word
    bind \cp up-or-search
    fzf_key_bindings
    if type -q atuin
      bind \cr _atuin_search
      bind -M insert \cr _atuin_search
    end
  end
end
