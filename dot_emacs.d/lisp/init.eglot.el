;; Enhanced LSP support with built-in eglot
(use-package eglot
  :ensure nil  ;; Built into Emacs 29+
  :defer t
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (json-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure))
  :config
  ;; Performance tweaks
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-extend-to-xref t)

  ;; Add additional LSP servers configurations
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))

  ;; Nix language server
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nixd")))

  ;; Bash/Shell language server
  (add-to-list 'eglot-server-programs
               '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))

;; Nix mode
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure))

;; Enhanced error checking
(use-package flymake
  :ensure nil  ;; Built into Emacs
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-fringe-indicator-position 'right-fringe))

;; Eldoc enhancements for better documentation display
(use-package eldoc
  :ensure nil  ;; Built into Emacs
  :custom
  (eldoc-idle-delay 0.2)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

;; Treesit is built-in and doesn't need use-package
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (setq treesit-font-lock-level 4))

;; Auto-format on save - disabled to avoid dependency issues
;; (use-package apheleia
;;   :hook (prog-mode . apheleia-mode)
;;   :config
;;   (setq apheleia-formatters
;;         '((black . ("black" "-"))
;;           (prettier . ("prettier" "--stdin-filepath" filepath))
;;           (rustfmt . ("rustfmt" "--edition" "2021"))
;;           (gofmt . ("gofmt"))
;;           (clang-format . ("clang-format" "-style=file"))
;;           (shfmt . ("shfmt" "-i" "2" "-ci"))
;;           (terraform . ("terraform" "fmt" "-"))))
;;   
;;   (setq apheleia-mode-alist
;;         '((python-mode . black)
;;           (python-ts-mode . black)
;;           (js-mode . prettier)
;;           (js-ts-mode . prettier)
;;           (typescript-mode . prettier)
;;           (typescript-ts-mode . prettier)
;;           (tsx-ts-mode . prettier)
;;           (json-mode . prettier)
;;           (json-ts-mode . prettier)
;;           (yaml-mode . prettier)
;;           (yaml-ts-mode . prettier)
;;           (css-mode . prettier)
;;           (css-ts-mode . prettier)
;;           (html-mode . prettier)
;;           (html-ts-mode . prettier)
;;           (markdown-mode . prettier)
;;           (rust-mode . rustfmt)
;;           (rust-ts-mode . rustfmt)
;;           (go-mode . gofmt)
;;           (go-ts-mode . gofmt)
;;           (c-mode . clang-format)
;;           (c++-mode . clang-format)
;;           (c-ts-mode . clang-format)
;;           (c++-ts-mode . clang-format)
;;           (sh-mode . shfmt)
;;           (terraform-mode . terraform))))

(provide 'init.eglot)