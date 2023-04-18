(setq recentf-max-saved-items 100)

;; company
(use-package company
  :init
  (global-company-mode 1) ; Enable company mode globally
  :config
  (setq company-idle-delay 0.1) ; Set completion menu to show quickly
  (setq company-minimum-prefix-length 2) ; Set minimum prefix length for completion
  (setq company-selection-wrap-around t) ; Wrap around selection when reaching the first or last item
  (setq company-tooltip-align-annotations t) ; Align annotations to the right tooltip border
  (setq company-require-match 'never) ; Allow free typing even if there's no match
  :bind
  (:map company-active-map
        ("C-n" . company-select-next) ; Use C-n and C-p for navigating the completion menu
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer) ; Use C-d to show documentation
        ("M-SPC" . company-complete))) ; Use <tab> to complete the current selection

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))

;; yasnippet
(setq-default abbrev-mode 1)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer)

(use-package ivy-yasnippet
  :bind ("C-c y" . ivy-yasnippet))

;; lsp
(use-package lsp-mode
  :hook ((c-mode c++-mode d-mode elm-mode go-mode js-mode kotlin-mode python-mode
          typescript-mode vala-mode web-mode)
         . lsp)
  :init
  (setq lsp-keymap-prefix "H-l"
        lsp-rust-analyzer-proc-macro-enable t)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-show-code-actions t)
  :bind (("C-c A" . lsp-execute-code-action)
         ("C-c d" . lsp-ui-doc-show)
         ("C-c I" . lsp-ui-imenu)))

(provide 'init.completion)
