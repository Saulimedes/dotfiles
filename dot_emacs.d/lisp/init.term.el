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
  (vterm-shell "/usr/bin/fish")  ; Replace with your preferred shell
  :config
  ;; Make vterm files directory tracking work with fish
  (setq vterm-tramp-shells '(("ssh" "/bin/bash")))
  
  ;; Set evil to start in emacs state in vterm
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs)))

;; Multiple vterms management
(use-package multi-vterm
  :after vterm
  :bind
  (("C-c t t" . multi-vterm-project)
   ("C-c t n" . multi-vterm-next)
   ("C-c t p" . multi-vterm-prev)))

;; Fish shell syntax highlighting
(use-package fish-mode
  :mode "\\.fish\\'")

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

;; Keybindings to open vterm in horizontal or vertical split
(global-set-key (kbd "C-c t h") 'split-horizontal-and-run-vterm)
(global-set-key (kbd "C-c t v") 'split-vertical-and-run-vterm)

;; Project-aware terminal
(defun vterm-project-root ()
  "Open vterm in the project root."
  (interactive)
  (if (fboundp 'projectile-project-root)
      (let ((default-directory (projectile-project-root)))
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

;; Add evil friendly keybindings
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    ;; Terminal prefix menu
    (kbd "<leader>t") '(:ignore t :which-key "terminal")
    
    ;; VTerm commands
    (kbd "<leader>tv") '(:ignore t :which-key "vterm")
    (kbd "<leader>tvp") 'multi-vterm-project
    (kbd "<leader>tvv") 'split-vertical-and-run-vterm
    (kbd "<leader>tvs") 'split-horizontal-and-run-vterm
    (kbd "<leader>tvr") 'vterm-project-root)
  
  ;; Update which-key descriptions
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "SPC t" "terminal")
    (which-key-add-key-based-replacements "SPC tv" "vterm")))

(provide 'init.term)
