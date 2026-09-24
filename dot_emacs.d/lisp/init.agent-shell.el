;; -*- lexical-binding: t; -*-
;; agent-shell - ACP (Agent Client Protocol) client, for driving opencode's
;; own agent inside an Emacs buffer instead of a terminal TUI. Separate from
;; gptel (init.gptel.el): this drives opencode itself, not a raw LLM call.
;; Auth is intentionally left to `opencode auth login' - agent-shell-opencode-
;; authentication defaults to :none, so opencode's own auth.json is used and
;; nothing is stored here.

(use-package agent-shell
  :commands (agent-shell-opencode-start-agent)
  :init
  ;; 'prompt (the default) asks which session to resume/start via a
  ;; minibuffer read from inside an async ACP response callback - with more
  ;; than a couple of prior sessions for a directory this throws "Command
  ;; attempted to use minibuffer while in minibuffer" and the shell just
  ;; hangs at "Starting agent" forever, no error surfaced in the buffer.
  ;; 'new sidesteps the whole picker.
  (setq agent-shell-session-strategy 'new))

(global-set-key (kbd "C-c A o") #'agent-shell-opencode-start-agent)
;; Easy default entry point - opencode is the primary AI assistant for now,
;; gptel (C-c A g/r/c) stays as a fallback once its Anthropic key is set up.
(global-set-key (kbd "C-c C-a") #'agent-shell-opencode-start-agent)

(provide 'init.agent-shell)
