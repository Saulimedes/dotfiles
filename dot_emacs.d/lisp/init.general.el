;; General.el - A more convenient key-binding system
(use-package general
  :config
  ;; Create a leader key definer for Evil mode
  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  ;; Create a local leader key definer (for major mode specific commands)
  (general-create-definer my/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "C-,")
  
  ;; Define global leader key bindings
  (my/leader-keys
    ;; Top level commands
    "SPC" '(execute-extended-command :which-key "M-x")
    ";" '(eval-expression :which-key "eval expression")
    ":" '(eval-expression :which-key "eval expression")
    "." '(find-file :which-key "find file")
    
    ;; File operations
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save file")
    "fS" '(write-file :which-key "save as")
    "fr" '(consult-recent-file :which-key "recent files")
    "fd" '(dired :which-key "dired")
    
    ;; Buffer operations
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bd" '(kill-current-buffer :which-key "kill buffer")
    "bD" '(kill-buffer-and-window :which-key "kill buffer and window")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "br" '(revert-buffer :which-key "revert buffer")
    "bs" '(basic-save-buffer :which-key "save buffer")
    
    ;; Window operations
    "w"  '(:ignore t :which-key "windows")
    "ws" '(split-window-below :which-key "split window below")
    "wv" '(split-window-right :which-key "split window right")
    "ww" '(other-window :which-key "other window")
    "wd" '(delete-window :which-key "delete window")
    "wm" '(delete-other-windows :which-key "maximize window")
    "wh" '(evil-window-left :which-key "window left")
    "wj" '(evil-window-down :which-key "window down")
    "wk" '(evil-window-up :which-key "window up")
    "wl" '(evil-window-right :which-key "window right")
    
    ;; Search operations
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "search in buffer")
    "sp" '(consult-ripgrep :which-key "search in project")
    "sd" '(consult-find :which-key "find files")
    
    ;; Project operations
    "p"  '(:ignore t :which-key "projects")
    "pf" '(project-find-file :which-key "find file in project")
    "pp" '(project-switch-project :which-key "switch project")
    "pb" '(project-switch-to-buffer :which-key "switch to project buffer")
    "pk" '(project-kill-buffers :which-key "kill project buffers")
    "ps" '(consult-ripgrep :which-key "search in project")
    
    ;; Code operations
    "c"  '(:ignore t :which-key "code")
    "cd" '(xref-find-definitions :which-key "find definition")
    "cD" '(xref-find-definitions-other-window :which-key "find definition other window")
    "cr" '(xref-find-references :which-key "find references")
    "cn" '(flymake-goto-next-error :which-key "next error")
    "cp" '(flymake-goto-prev-error :which-key "previous error")
    "cf" '(eglot-format-buffer :which-key "format buffer")
    "ca" '(eglot-code-actions :which-key "code actions")
    "cr" '(eglot-rename :which-key "rename symbol")
    
    ;; Toggle operations
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(visual-line-mode :which-key "toggle line wrap")
    "tn" '(display-line-numbers-mode :which-key "toggle line numbers")
    "tw" '(whitespace-mode :which-key "toggle whitespace")
    "th" '(hl-line-mode :which-key "toggle highlight line")
    
    ;; Help
    "h"  '(:ignore t :which-key "help")
    "hf" '(helpful-callable :which-key "describe function")
    "hv" '(helpful-variable :which-key "describe variable")
    "hk" '(helpful-key :which-key "describe key")
    "hm" '(helpful-macro :which-key "describe macro")
    "hp" '(helpful-at-point :which-key "help at point")
    "hi" '(info :which-key "info")
    
    ;; Application shortcuts
    "a"  '(:ignore t :which-key "apps")
    "ad" '(dired :which-key "dired")
    "at" '(vterm :which-key "terminal")
    "ag" '(magit-status :which-key "magit")
    "ao" '(org-agenda :which-key "org agenda")
    "ac" '(calc :which-key "calculator")
    "ar" '(recentf-open-files :which-key "recent files")))

(provide 'init.general)