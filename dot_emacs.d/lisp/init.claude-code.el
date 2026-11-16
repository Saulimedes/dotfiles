;; -*- lexical-binding: t; -*-
;; claude-code-ide.el - runs the Claude Code CLI in a vterm side window and
;; connects it to Emacs over MCP (current file/selection, diagnostics from
;; flymake/eglot, ediff for proposed changes, xref/imenu/treesit lookups).
;; Separate from agent-shell (opencode, init.agent-shell.el) and gptel
;; (init.gptel.el). Leader keys live under SPC a in init.keybindings.el.
;;
;; In a Claude buffer: M-RET = newline in the prompt, C-<escape> = send ESC
;; (plain ESC leaves meow's insert state, like in any other vterm), C-c C-x =
;; clear the shared selection. The C-c C-' binding is GUI-only (a tty cannot
;; send C-'), so use SPC a a in emacsclient -t frames.

;; The daemon is sometimes (re)started from inside a Claude Code session, and
;; then carries that session's identity in its environment (CLAUDECODE,
;; CLAUDE_CODE_SESSION_ID, CLAUDE_CODE_CHILD_SESSION, the messaging socket...).
;; Every vterm/eshell and every `claude' started from here would inherit it:
;; the CLI treats itself as a child of a foreign session, and shell configs
;; that skip setup under $CLAUDECODE (gpg tty) skip it in plain terminals too.
;; Settings the user exports on purpose (CLAUDE_CODE_USE_*, API keys) stay.
(setq process-environment
      (seq-remove
       (lambda (var)
         (string-match-p
          (concat "\\`\\(CLAUDECODE\\|CLAUDE_PID\\|CLAUDE_EFFORT\\|CLAUDE_CODE_"
                  "\\(ENTRYPOINT\\|EXECPATH\\|SESSION_ID\\|SESSION_ATTENDED"
                  "\\|CHILD_SESSION\\|BRIDGE_SESSION_ID\\|MESSAGING_[A-Z_]+\\)\\)=")
          var))
       process-environment))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'vterm)
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 100)
  ;; vterm stays: it is already built and set up; this only silences the
  ;; once-per-session hint to try ghostel.
  (claude-code-ide-show-backend-recommendation nil)
  ;; Off on purpose: with it on, Claude can evaluate arbitrary Elisp in this
  ;; Emacs through MCP, outside the CLI's own permission prompts.
  (claude-code-ide-enable-execute-code nil)
  :config
  ;; Read-only MCP tools: xref-find-references/apropos, treesit-info,
  ;; imenu-list-symbols, project-info.
  (claude-code-ide-emacs-tools-setup))

(provide 'init.claude-code)
