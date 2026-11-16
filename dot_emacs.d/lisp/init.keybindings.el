;; -*- lexical-binding: t; -*-
;; Keybindings: Meow modal editing + Leader keys

;; ============================================================
;; Meow - Native Emacs Modal Editing
;; ============================================================

(use-package meow
  :demand t
  :custom
  ;; y/s/p use the system clipboard: without this meow rebinds
  ;; `select-enable-clipboard' to nil, so copies never reach other apps.
  (meow-use-clipboard t)
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

     ;; Same letters as the dashboard shortcuts (the others, a c p s t q, are
     ;; groups here: agenda/capture/term are SPC o a/c/e, SPC t e)
     '("r" . consult-recent-file)
     '("m" . magit-status)
     '("v" . vterm-project-root)

     ;; AI (gptel + agent-shell/opencode)
     '("a" . (keymap))
     '("ag" . gptel)
     '("ar" . gptel-rewrite)
     '("ac" . my/gptel-add-comments)
     '("ae" . my/gptel-explain-code)
     '("ao" . agent-shell-opencode-start-agent)
     '("aa" . claude-code-ide-menu)
     '("ai" . claude-code-ide)
     '("at" . claude-code-ide-toggle)
     '("ap" . claude-code-ide-send-prompt)
     '("am" . claude-code-ide-insert-at-mentioned)
     '("ab" . claude-code-ide-switch-to-buffer)
     '("aq" . claude-code-ide-stop)

     ;; Files: f finds a file directly and e opens the tree, same letters as
     ;; the dashboard shortcuts. The rest of the file commands are under F.
     '("f" . find-file)
     '("e" . dirvish-side)
     '("F" . (keymap))
     '("Ff" . find-file)
     '("Fr" . consult-recent-file)
     '("Fs" . save-buffer)
     '("FS" . write-file)
     '("Fd" . dired)
     '("FD" . dired-jump)
     '("Fy" . my/copy-file-path)

     ;; Buffers
     '("b" . (keymap))
     '("bb" . consult-buffer)
     '("bd" . my/kill-buffer-quick)
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
     '("sh" . consult-history)
     '("sr" . query-replace)
     '("sR" . query-replace-regexp)

     ;; Project: SPC p is projectile-command-map. init.projectile.el loads
     ;; after this file and its :bind-keymap on C-c p replaces any "p" group
     ;; defined here, so a group here is dead on every fresh start.

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
     '("gj" . majutsu)

     ;; Toggle
     '("t" . (keymap))
     '("tt" . modus-themes-toggle)
     '("tT" . consult-theme)
     '("tl" . display-line-numbers-mode)
     '("tw" . visual-line-mode)
     '("th" . hl-line-mode)
     '("tf" . flymake-mode)
     '("ti" . highlight-indent-guides-mode)
     '("tz" . delete-trailing-whitespace)
     '("te" . eshell)

     ;; Help
     '("h" . (keymap))
     '("hf" . helpful-callable)
     '("hv" . helpful-variable)
     '("hk" . helpful-key)
     '("hm" . describe-mode)
     '("hp" . helpful-at-point)
     '("hi" . info)
     '("hb" . embark-bindings)
     '("hc" . consult-mode-command)

     ;; Apps/Open
     '("o" . (keymap))
     '("oe" . eshell)
     '("oE" . eshell-here)
     '("oT" . eshell-toggle)
     '("oS" . split-and-eshell-here)
     '("oP" . eshell-project-root)
     '("ot" . vterm)
     '("od" . dirvish-dwim)
     '("os" . dirvish-side)
     '("ow" . wcopy-toggle)
     '("oa" . org-agenda)
     '("oc" . org-capture)
     '("of" . make-frame)

     ;; Insert (was C-c e: SPC e is the tree now)
     '("i" . (keymap))
     '("iu" . insert-char)
     '("ie" . emojify-insert-emoji)
     '("is" . emoji-search)
     '("id" . emojify-describe-emoji-at-point)
     '("il" . emojify-list-emojis)

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

;; Copy and paste: y copies the selection, or the current line when nothing is
;; selected (like C-c in VS Code); p then pastes such a line above the current
;; one instead of splitting it mid-line.
(defvar my/copied-line nil "Text of the last line copied by `my/copy-line'.")

(defun my/copy-line ()
  "Copy the current line, newline included."
  (interactive)
  (let ((text (buffer-substring (line-beginning-position) (line-beginning-position 2)))
        (select-enable-clipboard meow-use-clipboard))
    (kill-new (if (string-suffix-p "\n" text) text (concat text "\n")))
    (setq my/copied-line (car kill-ring))
    (message "Copied line")))

(setf (alist-get 'meow-save meow-selection-command-fallback) #'my/copy-line)

(define-advice meow-yank (:before (&rest _) linewise)
  (when (and my/copied-line (not (region-active-p))
             (equal (current-kill 0 t) my/copied-line))
    (beginning-of-line)))

;; Middle click pastes at the click: the mouse selection if there is one, else
;; the kill ring (terminal frames have no primary selection to read).
(defun my/mouse-paste (event)
  "Paste at the position of EVENT."
  (interactive "e")
  (if (ignore-errors (gui-get-primary-selection))
      (mouse-yank-primary event)
    (mouse-set-point event)
    (yank)))

(global-set-key (kbd "<mouse-2>") #'my/mouse-paste)

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
  ;; The default is t whenever which-key loads without a GUI display (a
  ;; daemon), and the simple fit counts lines without `line-spacing', so the
  ;; last row of the popup was cut off.
  (which-key-allow-imprecise-window-fit nil)
  ;; The popup is a normal buffer: no relative line numbers in it
  :hook (which-key-init-buffer . (lambda () (setq-local display-line-numbers nil)))
  :config
  (which-key-mode)
  ;; Better descriptions
  (which-key-add-key-based-replacements
    "SPC a" "ai"
    "SPC F" "files"
    "SPC i" "insert"
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
;; Note: M-[ is the CSI escape prefix in terminals - never bind it globally
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)

;; Date/time insertion
(global-set-key (kbd "M-S-d") 'my/insert-current-date)
(global-set-key (kbd "M-S-t") 'my/insert-current-time)

;; ============================================================
;; Utility Functions
;; ============================================================

(defun my/copy-file-path ()
  "Copy the current buffer's file path to clipboard."
  (interactive)
  (if-let* ((path (or (buffer-file-name) default-directory)))
      (progn
        (kill-new path)
        (message "Copied: %s" path))
    (message "No file path")))

(defun my/insert-current-date ()
  "Insert current date in German format (DD.MM.YYYY)."
  (interactive)
  (insert (format-time-string "%d.%m.%Y")))

(defun my/insert-current-time ()
  "Insert current time (HH:MM)."
  (interactive)
  (insert (format-time-string "%H:%M")))

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

;; Winner mode for window undo/redo
(winner-mode 1)

(provide 'init.keybindings)
