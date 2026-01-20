;; -*- lexical-binding: t; -*-
;; Modern completion stack: Vertico + Consult + Corfu + Cape

;; ============================================================
;; Minibuffer Completion: Vertico
;; ============================================================

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-l" . vertico-insert)
              ("C-u" . vertico-scroll-down)
              ("C-d" . vertico-scroll-up)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)
  (vertico-count 12)
  (vertico-resize nil)
  (vertico-scroll-margin 2)
  :init
  (vertico-mode)
  ;; Vertico extensions
  (vertico-multiform-mode)
  :config
  ;; Different display for different commands
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer)
          (consult-git-grep buffer)
          (consult-grep buffer)
          (consult-imenu buffer)
          (consult-outline buffer))))

;; Directory navigation extension
(use-package vertico-directory
  :after vertico
  :ensure nil
  :load-path "straight/build/vertico/extensions/"
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; ============================================================
;; Fuzzy Matching: Orderless
;; ============================================================

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal
                               orderless-prefixes
                               orderless-initialism
                               orderless-regexp)))

;; ============================================================
;; Rich Annotations: Marginalia
;; ============================================================

(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :init
  (marginalia-mode))

;; ============================================================
;; Enhanced Commands: Consult
;; ============================================================

(use-package consult
  :demand t
  :bind (;; C-c bindings
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; M-g bindings (goto)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Other
         ("C-s" . consult-line)
         ("M-y" . consult-yank-pop))
  :custom
  (consult-narrow-key "<")
  (consult-project-root-function #'project-root)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Preview on any key
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; ============================================================
;; Context Actions: Embark
;; ============================================================

(use-package embark
  :demand t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================
;; In-Buffer Completion: Corfu
;; ============================================================

(use-package corfu
  :demand t
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-l" . corfu-insert)
              ("C-g" . corfu-quit)
              ("M-d" . corfu-popupinfo-toggle)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; Completion sources
(use-package cape
  :demand t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; Icons for corfu
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ============================================================
;; Better Help
;; ============================================================

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h o" . helpful-symbol)
         ("C-c C-d" . helpful-at-point)))

;; ============================================================
;; Recent Files
;; ============================================================

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode))

;; ============================================================
;; Snippets: Tempel (faster, simpler than yasnippet)
;; ============================================================

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :bind (:map tempel-map
              ("M-]" . tempel-next)
              ("M-[" . tempel-previous)
              ("C-g" . tempel-abort))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'org-mode-hook 'tempel-setup-capf))

;; Community template collection
(use-package tempel-collection
  :after tempel)

(provide 'init.completion)
