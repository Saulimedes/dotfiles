;; God Mode - Efficient Modal Editing for Emacs
(use-package god-mode
  :init
  (setq god-mode-enable-function-key-translation nil
        god-mode-alist '((nil . "C-")
                        ("g" . "M-")
                        ("G" . "C-M-")))
  :config
  ;; Toggle god-mode with escape
  (global-set-key (kbd "<escape>") #'god-local-mode)
  
  ;; Cursor color changes based on mode
  (defun my/god-mode-update-cursor-type ()
    "Change cursor appearance according to god-mode state."
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'box
                        'bar)))
  
  (add-hook 'post-command-hook #'my/god-mode-update-cursor-type)
  
  ;; Exempted major modes where we don't want god-mode
  (dolist (mode '(term-mode
                  vterm-mode
                  eshell-mode
                  magit-status-mode
                  dired-mode))
    (add-to-list 'god-exempt-major-modes mode))

  ;; Different cursor colors for different modes
  (defun my/god-mode-update-cursor-color ()
    "Change cursor color according to god-mode state."
    (let ((color (if god-local-mode "#D4AF37" "#00BFFF")))
      (set-cursor-color color)))
  
  (add-hook 'god-mode-enabled-hook #'my/god-mode-update-cursor-color)
  (add-hook 'god-mode-disabled-hook #'my/god-mode-update-cursor-color)
  
  ;; Easy way to enable god-mode everywhere
  (global-set-key (kbd "C-c z") 'god-mode-all)
  
  ;; Add the mode line indicator for god-mode
  (defun my/god-mode-update-mode-line ()
    "Update the mode line with the god-mode status."
    (cond (god-local-mode
           (set-face-attribute 'mode-line nil
                              :background "#E5C07B"
                              :foreground "black")
           (message "GOD mode enabled"))
          (t
           (set-face-attribute 'mode-line nil
                              :background "#21242B"
                              :foreground "#BBBBBB")
           (message "GOD mode disabled"))))
  
  (add-hook 'god-mode-enabled-hook #'my/god-mode-update-mode-line)
  (add-hook 'god-mode-disabled-hook #'my/god-mode-update-mode-line)
  
  ;; Better isearch integration for god-mode
  (define-key god-local-mode-map [remap isearch-forward] 'isearch-forward)
  (define-key god-local-mode-map [remap isearch-backward] 'isearch-backward)
  
  ;; Add some useful god-mode specific keybindings
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)
  (define-key god-local-mode-map (kbd ".") 'repeat)
  
  ;; Enable global god-mode by default for a more Vim-like start
  (god-mode-all))

;; Comment/uncomment lines - replacement for evil-commentary
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2))
  :config
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

;; Electric pair mode - built-in replacement for evil-surround
(use-package elec-pair
  :ensure nil  ;; built-in
  :hook (prog-mode . electric-pair-mode)
  :config
  (setq electric-pair-preserve-balance t
        electric-pair-skip-self 'electric-pair-default-skip-self
        electric-pair-inhibit-predicate 'electric-pair-default-inhibit))

;; Useful Emacs keybindings
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-set)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Add some convenient keyboard shortcuts with god-mode in mind
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-c SPC") 'just-one-space)

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

;; Which-Key for discoverability
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-setup-side-window-bottom)
  ;; Special prefix descriptions for god-mode
  (which-key-add-key-based-replacements
    "C-x" "files & buffers"
    "C-c" "mode specific"
    "C-c g" "git commands"
    "C-c t" "toggles"
    "C-h" "help"
    "M-g" "goto/movement"))

;; Theme and git-related keybindings using god-mode compatible bindings
(global-set-key (kbd "C-c t t") 'my/cycle-theme)
(global-set-key (kbd "C-c t g") 'git-gutter-mode)
(global-set-key (kbd "C-c t b") 'blamer-mode)

;; Git commands with C-c g prefix
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)
(global-set-key (kbd "C-c g l") 'my/git-show-current-line-blame)
(global-set-key (kbd "C-c g t") 'git-timemachine-toggle)
(global-set-key (kbd "C-c g d") 'magit-diff-unstaged)

;; Update which-key descriptions
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c t" "toggles")
  (which-key-add-key-based-replacements "C-c g" "git"))

(provide 'init.keybindings)
