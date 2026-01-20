;; -*- lexical-binding: t; -*-
;; Keybindings: Meow modal editing + Leader keys

;; ============================================================
;; Meow - Native Emacs Modal Editing
;; ============================================================

(use-package meow
  :demand t
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    ;; Motion state (for special modes)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))

    ;; Leader key (SPC)
    (meow-leader-define-key
     ;; Digit arguments
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

     ;; Top-level
     '("SPC" . execute-extended-command)
     '("." . find-file)
     '("," . consult-buffer)
     '("/" . consult-ripgrep)
     '("?" . meow-cheatsheet)
     '(";" . eval-expression)
     '("'" . vertico-repeat)
     '("u" . universal-argument)

     ;; Files
     '("f" . (keymap))
     '("ff" . find-file)
     '("fr" . consult-recent-file)
     '("fs" . save-buffer)
     '("fS" . write-file)
     '("fd" . dired)
     '("fD" . dired-jump)
     '("fy" . my/copy-file-path)

     ;; Buffers
     '("b" . (keymap))
     '("bb" . consult-buffer)
     '("bd" . kill-current-buffer)
     '("bD" . kill-buffer-and-window)
     '("bn" . next-buffer)
     '("bp" . previous-buffer)
     '("br" . revert-buffer-quick)
     '("bs" . save-buffer)
     '("bS" . save-some-buffers)
     '("bm" . consult-bookmark)

     ;; Windows
     '("w" . (keymap))
     '("ws" . split-window-below)
     '("wv" . split-window-right)
     '("wd" . delete-window)
     '("wD" . delete-other-windows)
     '("ww" . other-window)
     '("wh" . windmove-left)
     '("wj" . windmove-down)
     '("wk" . windmove-up)
     '("wl" . windmove-right)
     '("w=" . balance-windows)
     '("wu" . winner-undo)
     '("wr" . winner-redo)

     ;; Search
     '("s" . (keymap))
     '("ss" . consult-line)
     '("sS" . consult-line-multi)
     '("sp" . consult-ripgrep)
     '("sd" . consult-find)
     '("si" . consult-imenu)
     '("sI" . consult-imenu-multi)
     '("so" . consult-outline)
     '("sm" . consult-mark)
     '("sr" . query-replace)
     '("sR" . query-replace-regexp)

     ;; Project
     '("p" . (keymap))
     '("pp" . project-switch-project)
     '("pf" . project-find-file)
     '("pb" . consult-project-buffer)
     '("ps" . consult-ripgrep)
     '("pd" . project-dired)
     '("pk" . project-kill-buffers)
     '("pc" . project-compile)
     '("pe" . project-eshell)

     ;; Code/LSP
     '("c" . (keymap))
     '("cd" . xref-find-definitions)
     '("cD" . xref-find-definitions-other-window)
     '("cr" . xref-find-references)
     '("cs" . consult-eglot-symbols)
     '("ca" . eglot-code-actions)
     '("cf" . eglot-format-buffer)
     '("cR" . eglot-rename)
     '("cn" . flymake-goto-next-error)
     '("cp" . flymake-goto-prev-error)
     '("ci" . consult-imenu)

     ;; Git
     '("g" . (keymap))
     '("gg" . magit-status)
     '("gs" . magit-status)
     '("gb" . magit-blame)
     '("gl" . magit-log-current)
     '("gL" . magit-log-buffer-file)
     '("gd" . magit-diff-unstaged)
     '("gD" . magit-diff-staged)
     '("gc" . magit-commit)
     '("gp" . magit-push)
     '("gf" . magit-fetch)
     '("gF" . magit-pull)

     ;; Toggle
     '("t" . (keymap))
     '("tt" . consult-theme)
     '("tl" . display-line-numbers-mode)
     '("tw" . visual-line-mode)
     '("th" . hl-line-mode)
     '("tf" . flymake-mode)
     '("ti" . highlight-indent-guides-mode)
     '("tz" . delete-trailing-whitespace)

     ;; Help
     '("h" . (keymap))
     '("hf" . helpful-callable)
     '("hv" . helpful-variable)
     '("hk" . helpful-key)
     '("hm" . describe-mode)
     '("hp" . helpful-at-point)
     '("hi" . info)
     '("hb" . embark-bindings)

     ;; Apps/Open
     '("o" . (keymap))
     '("oe" . eshell)
     '("oE" . eshell-here)
     '("ot" . vterm)
     '("od" . dired-jump)
     '("oa" . org-agenda)
     '("oc" . org-capture)
     '("of" . make-frame)

     ;; Quit/Session
     '("q" . (keymap))
     '("qq" . save-buffers-kill-terminal)
     '("qr" . restart-emacs)
     '("qd" . my/show-dashboard))

    ;; Normal state keys
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
     '("U" . undo-redo)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("/" . consult-line)
     '("<escape>" . ignore)))

  (meow-setup)
  (meow-global-mode 1)

  ;; Cursor styles per state
  (setq meow-cursor-type-normal 'box
        meow-cursor-type-insert '(bar . 2)
        meow-cursor-type-motion 'hollow)

  ;; Use insert state for these modes
  (dolist (mode '(eshell-mode
                  vterm-mode
                  term-mode
                  git-rebase-mode
                  magit-log-edit-mode))
    (add-to-list 'meow-mode-state-list (cons mode 'insert))))

;; ============================================================
;; Which-Key - Keybinding hints
;; ============================================================

(use-package which-key
  :demand t
  :custom
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.25)
  (which-key-add-column-padding 1)
  (which-key-separator " → ")
  :config
  (which-key-mode)
  ;; Better descriptions
  (which-key-add-key-based-replacements
    "SPC f" "files"
    "SPC b" "buffers"
    "SPC w" "windows"
    "SPC s" "search"
    "SPC p" "project"
    "SPC c" "code"
    "SPC g" "git"
    "SPC t" "toggle"
    "SPC h" "help"
    "SPC o" "open"
    "SPC q" "quit"))

;; ============================================================
;; Global Keybindings
;; ============================================================

;; Escape quits everything
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Text scaling
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Window navigation (also works in insert mode)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)

;; Buffer navigation
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;; ============================================================
;; Utility Functions
;; ============================================================

(defun my/copy-file-path ()
  "Copy the current buffer's file path to clipboard."
  (interactive)
  (if-let ((path (or (buffer-file-name) default-directory)))
      (progn
        (kill-new path)
        (message "Copied: %s" path))
    (message "No file path")))

(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (move-beginning-of-line 1)
      (kill-line)
      (yank)
      (newline)
      (yank))
    (move-to-column col)))

(defun eshell-here ()
  "Open eshell in current directory."
  (interactive)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               default-directory)))
    (eshell 'new)
    (eshell/cd dir)
    (eshell-send-input)))

;; Winner mode for window undo/redo
(winner-mode 1)

(provide 'init.keybindings)
