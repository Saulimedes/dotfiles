;; Helm - Incremental completion and selection narrowing framework
(use-package helm
  :init
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-scroll-amount 4
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20
        helm-buffer-skip-remote-checking t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-limit 150
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("C-x C-r" . helm-recentf)
   ("C-s" . helm-occur)
   ("M-y" . helm-show-kill-ring)
   ("C-c h" . helm-command-prefix))
  :bind
  (:map helm-map
        ("<tab>" . helm-execute-persistent-action)
        ("C-i" . helm-execute-persistent-action)
        ("C-z" . helm-select-action)))

;; Helm Swoop - Better search in buffers
(use-package helm-swoop
  :after helm
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))
  :config
  (setq helm-multi-swoop-edit-save t
        helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-speed-or-color nil
        helm-swoop-move-to-line-cycle t
        helm-swoop-use-line-number-face t
        helm-swoop-use-fuzzy-match t))

;; Helm Projectile integration
(use-package helm-projectile
  :after (helm projectile)
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm
        projectile-indexing-method 'alien
        projectile-switch-project-action #'helm-projectile-find-file))

;; Helm Descbinds - Better describe-bindings
(use-package helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode))

;; Enhanced completion with Corfu (fallback for in-buffer completion)
(use-package corfu
  :custom
  (corfu-cycle t)                   ; Cycling through candidates
  (corfu-auto t)                    ; Enable auto completion
  (corfu-auto-prefix 2)             ; Complete with 2 prefix chars
  (corfu-auto-delay 0.1)            ; Very small delay before popup
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)   ; Preview the current candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)        ; Don't auto-confirm exact matches
  (corfu-popupinfo-delay '(0.2 . 0.1)) ; Show docs quickly
  :init
  (global-corfu-mode)
  :config
  ;; Enable Corfu more generally
  (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil)
                               (corfu-mode)))
  (add-hook 'minibuffer-setup-hook #'corfu-mode)
  
  ;; TAB cycle if there are only few candidates
  (setq tab-always-indent 'complete)
  
  ;; Enable popup documentation
  (corfu-popupinfo-mode 1)
  
  ;; Use Dabbrev with Corfu
  (use-package dabbrev
    :ensure nil
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand))
    :config
    (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
    ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
    (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)))

;; Add icons to completion
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Enhanced completion in minibuffer
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize t))

;; Better sorting and filtering
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; Brief annotations for minibuffer
(use-package marginalia
  :bind
  (:map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Recent files with better defaults
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  :config
  (recentf-mode))

;; Useful actions for completion
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Integration between Consult and Embark
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Powerful search and navigation commands
(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ("M-y" . consult-yank-pop)
   ("M-s r" . consult-ripgrep)
   ("M-s f" . consult-find)
   ("M-s g" . consult-grep)
   ("M-s l" . consult-line)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line))
  :custom
  (consult-project-root-function #'project-root)
  (completion-in-region-function #'consult-completion-in-region))

;; Enhanced help system
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h o" . helpful-symbol)
   ("C-h F" . helpful-function)
   ("C-c C-d" . helpful-at-point)))

;; Cape for better completion-at-point sources
(use-package cape
  :init
  ;; Add useful defaults completion sources from cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  
  ;; Setup Cape for different major modes
  :config
  ;; Set up file-specific completion backends
  (defun my/setup-completion-for-prog ()
    "Setup completion for programming modes"
    (setq-local completion-at-point-functions
                (list (cape-capf-buster #'eglot-completion-at-point)
                      #'cape-file
                      #'cape-dabbrev
                      #'cape-keyword)))
  
  (defun my/setup-completion-for-text ()
    "Setup completion for text modes"
    (setq-local completion-at-point-functions
                (list #'cape-dabbrev
                      #'cape-ispell  ; Spell checking
                      #'cape-dict    ; Dictionary words
                      #'cape-file)))
  
  (add-hook 'prog-mode-hook #'my/setup-completion-for-prog)
  (add-hook 'text-mode-hook #'my/setup-completion-for-text)
  
  ;; Enable LSP-specific completions with Eglot
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; Snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; Fallback to company for older Emacs versions
(use-package company
  :defer t
  :unless (>= emacs-major-version 28)
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-require-match 'never)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("M-SPC" . company-complete)))

(provide 'init.completion)
