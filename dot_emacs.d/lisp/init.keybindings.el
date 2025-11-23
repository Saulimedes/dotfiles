;; Meow - Modal editing for Emacs
(use-package meow
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  (meow-setup)
  (meow-global-mode 1))

;; General.el integration with Meow
(condition-case general-err
    (use-package general
      :after meow
  :config
  ;; Create a leader key definer for Meow mode
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
    "fr" '(helm-recentf :which-key "recent files")
    "fd" '(dired :which-key "dired")
    
    ;; Buffer operations
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(helm-mini :which-key "switch buffer")
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
    "wh" '(windmove-left :which-key "window left")
    "wj" '(windmove-down :which-key "window down")
    "wk" '(windmove-up :which-key "window up")
    "wl" '(windmove-right :which-key "window right")
    
    ;; Search operations
    "s"  '(:ignore t :which-key "search")
    "ss" '(helm-swoop :which-key "search in buffer")
    "sp" '(helm-projectile-rg :which-key "search in project")
    "sd" '(helm-find :which-key "find files")
    
    ;; Project operations
    "p"  '(:ignore t :which-key "projects")
    "pf" '(helm-projectile-find-file :which-key "find file in project")
    "pp" '(project-switch-project :which-key "switch project")
    "pb" '(project-switch-to-buffer :which-key "switch to project buffer")
    "pK" '(project-kill-buffers :which-key "kill project buffers") ; Changed from "pk" to "pK"
    "ps" '(helm-projectile-rg :which-key "search in project")
    
    ;; Code operations
    "c"  '(:ignore t :which-key "code")
    "cd" '(xref-find-definitions :which-key "find definition")
    "cD" '(xref-find-definitions-other-window :which-key "find definition other window")
    "cr" '(xref-find-references :which-key "find references")
    "cn" '(flymake-goto-next-error :which-key "next error")
    "cp" '(flymake-goto-prev-error :which-key "previous error")
    "cf" '(eglot-format-buffer :which-key "format buffer")
    "ca" '(eglot-code-actions :which-key "code actions")
    "cR" '(eglot-rename :which-key "rename symbol")
    
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
    "ar" '(recentf-open-files :which-key "recent files")
    
    ;; Git operations
    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "magit status")
    "gb" '(magit-blame :which-key "magit blame")
    "gl" '(magit-log :which-key "magit log")
    "gd" '(magit-diff-unstaged :which-key "magit diff")
    "gc" '(magit-commit :which-key "magit commit")
    "gp" '(magit-push :which-key "magit push")
    "gf" '(magit-fetch :which-key "magit fetch")))
  (error (message "General package not available: %s" (error-message-string general-err))))

;; Which-Key for discoverability
(condition-case which-key-err
    (use-package which-key
      :init
      (which-key-mode)
      :config
      (setq which-key-idle-delay 0.3)
      (which-key-setup-side-window-bottom))
  (error (message "Which-Key package not available: %s" (error-message-string which-key-err))))

;; Additional Meow keybindings
(with-eval-after-load 'meow
  ;; Use escape to quit various prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  
  ;; Better window navigation
  (global-set-key (kbd "C-h") 'windmove-left)
  (global-set-key (kbd "C-j") 'windmove-down)
  (global-set-key (kbd "C-k") 'windmove-up)
  (global-set-key (kbd "C-l") 'windmove-right)
  
  ;; Use insert state in modes that benefit from it
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'meow-mode-state-list (cons mode 'insert))))

;; Useful global keybindings that work well with Evil
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-set)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Custom functions
(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((col (current-column))
        (text (buffer-substring (line-beginning-position) (line-end-position))))
    (forward-line)
    (insert text "\n")
    (forward-line -1)
    (move-to-column col)))

;; Add duplicate line to meow normal mode
(with-eval-after-load 'meow
  (define-key meow-normal-state-keymap (kbd "gy") 'duplicate-line))

(provide 'init.keybindings)