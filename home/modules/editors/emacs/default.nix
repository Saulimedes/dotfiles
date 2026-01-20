# Emacs configuration
{ config, pkgs, lib, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk; # GTK + native compilation

    extraPackages = epkgs: with epkgs; [
      # =============================================
      # Essential packages
      # =============================================
      use-package
      which-key
      general
      meow  # Modal editing (replaces evil)
      gcmh  # Garbage collection magic hack

      # =============================================
      # Completion stack (modern Vertico ecosystem)
      # =============================================
      vertico
      orderless
      marginalia
      consult
      embark
      embark-consult
      corfu
      cape

      # =============================================
      # Snippets
      # =============================================
      tempel
      tempel-collection

      # =============================================
      # UI - Modern VSCode-like appearance
      # =============================================
      modus-themes
      catppuccin-theme
      ef-themes
      doom-modeline      # VSCode-like modeline
      solaire-mode       # Dim non-file buffers
      centaur-tabs       # VSCode-like tabs
      breadcrumb         # File path in header
      nerd-icons
      nerd-icons-corfu
      all-the-icons
      all-the-icons-completion
      pulsar             # Visual feedback (pulse line)
      beacon             # Cursor trail effect
      fontaine           # Font presets
      ligature           # Font ligatures
      symbol-overlay     # Highlight symbol at point
      rainbow-mode       # Colorize color strings
      rainbow-delimiters
      highlight-indent-guides
      page-break-lines
      visual-fill-column

      # =============================================
      # Git
      # =============================================
      magit
      diff-hl
      blamer
      git-timemachine
      git-gutter
      git-gutter-fringe
      browse-at-remote

      # =============================================
      # Programming / LSP
      # =============================================
      eglot  # Built-in LSP client
      flymake
      apheleia  # Async formatting
      treesit-grammars.with-all-grammars
      dumb-jump
      editorconfig
      format-all

      # =============================================
      # Languages
      # =============================================
      nix-mode
      markdown-mode
      yaml-mode
      json-mode
      rust-mode
      go-mode
      terraform-mode
      web-mode
      typescript-mode

      # =============================================
      # Org mode
      # =============================================
      org
      org-modern
      org-super-agenda
      org-bullets

      # =============================================
      # Project management & Nix integration
      # =============================================
      projectile
      treemacs
      treemacs-projectile
      envrc              # Direnv integration (preferred)
      direnv

      # =============================================
      # Terminal
      # =============================================
      vterm
      multi-vterm
      eshell-syntax-highlighting
      esh-autosuggest

      # =============================================
      # Utilities
      # =============================================
      helpful
      which-key
      hydra
      iedit
      multiple-cursors
      move-text
      hl-todo
      origami            # Code folding
      imenu-list

      # =============================================
      # AI assistance
      # =============================================
      copilot
      gptel
    ];

    extraConfig = ''
      ;; Copilot - inline code completions
      (use-package copilot
        :hook (prog-mode . copilot-mode)
        :bind (:map copilot-completion-map
                    ("TAB" . copilot-accept-completion)
                    ("C-TAB" . copilot-accept-completion-by-word)
                    ("C-n" . copilot-next-completion)
                    ("C-p" . copilot-previous-completion))
        :config
        (setq copilot-indent-offset-warning-disable t))

      ;; gptel - Claude and other LLM chat
      (use-package gptel
        :bind (("C-c g" . gptel)
               ("C-c G" . gptel-menu))
        :config
        (setq gptel-model 'claude-sonnet-4-20250514
              gptel-backend (gptel-make-anthropic "Claude"
                              :stream t
                              :key (lambda () (getenv "ANTHROPIC_API_KEY")))))
    '';
  };

  # Copy emacs config files (if you have existing elisp configs)
  # xdg.configFile."emacs" = {
  #   source = ./config;
  #   recursive = true;
  # };

  # Or use home.file for ~/.emacs.d
  # home.file.".emacs.d" = {
  #   source = ./config;
  #   recursive = true;
  # };
}
