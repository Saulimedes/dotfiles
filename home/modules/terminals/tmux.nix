# Tmux configuration
{ config, pkgs, lib, ... }:

{
  programs.tmux = {
    enable = true;
    shell = "${pkgs.fish}/bin/fish";
    terminal = "tmux-256color";
    historyLimit = 100000;
    baseIndex = 1;
    escapeTime = 10;
    keyMode = "vi";
    mouse = true;

    plugins = with pkgs.tmuxPlugins; [
      yank
      open
      copycat
      resurrect
      {
        plugin = online-status;
        extraConfig = ''
          set -g @online_icon "#[fg=brightblack, bg=brightgreen] "
          set -g @offline_icon "#[fg=brightblack, bg=brightred] "
          set -g @route_to_ping "1.1.1.1"
        '';
      }
      {
        # URL opening with fzf (wfxr/tmux-fzf-url)
        plugin = pkgs.tmuxPlugins.mkTmuxPlugin {
          pluginName = "tmux-fzf-url";
          version = "unstable-2024-01-01";
          src = pkgs.fetchFromGitHub {
            owner = "wfxr";
            repo = "tmux-fzf-url";
            rev = "1241fc1b0967d39d6d5a19dca8f7e59664e46e5f";
            sha256 = "sha256-UlrN4xM9VkBf2eHM7LoT3hKz+fJYU4H6eATRQGcP1Is=";
          };
        };
        extraConfig = ''
          set -g @fzf-url-fzf-options '-p 60%,30% --prompt="   " --border-label=" Open URL "'
          set -g @fzf-url-history-limit '2000'
        '';
      }
    ];

    extraConfig = ''
      # Terminal overrides for proper colors and features
      set -as terminal-overrides ',*:Smulx=\E[4::%p1%dm'
      set -as terminal-overrides ',*:Setulc=\E[58::2::%p1%{65536}%/%d::%p1%{256}%/%{255}%&%d::%p1%{255}%&%d%;m'
      set -ga terminal-overrides ",xterm-256color:RGB"

      # Cursor shape support
      set -ga terminal-overrides '*:Ss=\E[%p1%d q:Se=\E[ q'
      set -ga terminal-overrides '*:Cs=\E]12;%p1%s\007:Cr=\E]112\007'

      set -g allow-passthrough on
      setw -g xterm-keys on

      set -q -g status-utf8 on
      set -q -g utf8 on

      set -g visual-activity off
      set -g visual-bell off
      set -g visual-silence off
      set -g renumber-windows on
      set -g set-titles on
      set -g pane-base-index 1

      set-option -g set-titles on
      set-option -g set-titles-string "[#S] #T"

      # Pane navigation
      bind C-l send-keys 'C-l'
      bind-key -n C-h select-pane -L
      bind-key -n C-j select-pane -D
      bind-key -n C-k select-pane -U
      bind-key -n C-l select-pane -R

      # Open new windows/panes in current path
      bind c new-window -c '#{pane_current_path}'
      bind-key '"' split-window -h -c '#{pane_current_path}'
      bind-key % split-window -v -c '#{pane_current_path}'
      bind l select-layout main-vertical
      bind k switch-client -l

      set -g focus-events on

      # Copy mode
      bind-key -T copy-mode-vi 'v' send -X begin-selection
      bind-key -T copy-mode-vi 'y' send -X copy-selection-no-clear

      # Status bar
      set -g status-interval 60
      set -g status-style bg=terminal
      set-option -g status-justify right
      set-option -g status-left-length 160
      set-option -g status-fg colour7
      set-option -g status-bg colour0
      set -g status-right-length 150
      set-option -g status-left '#[bg=brightgreen]#[fg=black] #S #[bg=brightblack]#[fg=brightwhite] #H #[bg=black]'

      set -g status-justify centre
      set-window-option -g window-status-format "#[fg=cyan,bg=brightblack] #I #[fg=darkgrey]#W#[fg=colour5]#F #[bg=black]"
      set-window-option -g window-status-current-format "#[fg=black, bg=brightgreen] #I #[fg=brightwhite, bg=brightblack] #W#F #[bg=black]"
      set -g status-right "#[fg=brightwhite,bg=brightblack]#(kubectl config current-context 2>/dev/null || echo '')#(kubectl config view --minify --output 'jsonpath={..namespace}' 2>/dev/null | sed 's/^/ . /' || echo '') . %H:%M #{online_status}"

      set-option -g pane-active-border-style fg=colour2
      set-option -g pane-border-style fg=colour238

      # SSH agent forwarding
      set-hook -g pane-focus-in 'run-shell "if [ -n \"$(tmux showenv -g SSH_AUTH_SOCK | cut -d= -f2-)\" ]; then tmux setenv -g SSH_AUTH_SOCK $(tmux showenv -g SSH_AUTH_SOCK | cut -d= -f2-); fi"'

      # Yank settings
      set -g @yank_action 'copy-pipe'
      set -g @yank_selection 'clipboard'
      set -g @yank_with_mouse off
      set -g @yank_selection_mouse 'clipboard'

      # Clipboard command based on session type
      set -g @override_copy_command 'wl-copy'
      if-shell '[ -n "$DISPLAY" ] && [ -z "$WAYLAND_DISPLAY" ]' {
        set -g @override_copy_command 'xclip -selection clipboard'
      }

      # Resurrect settings
      set -g @resurrect-strategy-nvim 'session'
      set -g @resurrect-strategy-vim 'session'
      set -g @resurrect-capture-pane-contents 'on'
      set -g @resurrect-processes 'ssh kubectl ansible'
    '';
  };
}
