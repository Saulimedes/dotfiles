;; -*- lexical-binding: t; -*-
;; Modern Terminal Experience

;; Inherits environment variables from the shell
(use-package exec-path-from-shell
  :config
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))

;; VTerm - A better terminal emulator
(use-package vterm
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
  (vterm-shell "/usr/bin/zsh")  ; fish isn't installed on this system
  (vterm-always-compile-module t) ; auto-build the native module, no prompt
  :config
  ;; Make vterm files directory tracking work with zsh/bash/fish
  (setq vterm-tramp-shells '(("ssh" "/bin/bash")
                             ("ssh" "/usr/bin/zsh")
                             ("ssh" "/usr/bin/fish"))))

;; Multiple vterms management. "t" and "h" avoided under C-c t: meow uses
;; mode-specific-map (C-c) as its leader keymap, so SPC t t/th already claim
;; those two slots for consult-theme/hl-line-mode.
(use-package multi-vterm
  :after vterm
  :bind
  (("C-c t o" . multi-vterm-project)
   ("C-c t n" . multi-vterm-next)
   ("C-c t p" . multi-vterm-prev)))

;; Fish shell syntax highlighting removed - prioritizing zsh/bash
;; (use-package fish-mode
;;   :mode "\\.fish\\'")

;; Functions to run vterm in splits
(defun split-horizontal-and-run-vterm ()
  "Split the window horizontally and run vterm."
  (interactive)
  (split-window-below)
  (other-window 1)
  (vterm))

(defun split-vertical-and-run-vterm ()
  "Split the window vertically and run vterm."
  (interactive)
  (split-window-right)
  (other-window 1)
  (vterm))

;; Keybindings to open vterm in horizontal or vertical split (2/3 mirror
;; C-x 2/C-x 3's split meaning; C-c t h is claimed by meow's SPC t h).
(global-set-key (kbd "C-c t 2") 'split-horizontal-and-run-vterm)
(global-set-key (kbd "C-c t v") 'split-vertical-and-run-vterm)

;; Project-aware terminal
(defun vterm-project-root ()
  "Open vterm in the project root, or `default-directory' if not in one."
  (interactive)
  (if-let* (((fboundp 'projectile-project-root))
            (root (projectile-project-root)))
      (let ((default-directory root))
        (vterm))
    (vterm)))

(global-set-key (kbd "C-c t r") 'vterm-project-root)

;; Keep legacy terminal functionality
(use-package shell-pop
  :after vterm
  :custom
  (shell-pop-shell-type '("vterm" "*vterm*" (lambda () (vterm))))
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom")
  (shell-pop-restore-window-configuration t)
  (shell-pop-autocd-to-working-dir t))

(provide 'init.term)
