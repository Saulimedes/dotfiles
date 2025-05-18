;; Enhanced project management with Projectile
(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-project-search-path '("~/Projects"))
  (setq projectile-switch-project-action #'projectile-find-file)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'default) ; Use Vertico/Marginalia instead of Ivy
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-directories
        (append '(".git" ".idea" ".vscode" "node_modules" "build" "dist" "target")
                projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files
        (append '("*.gz" "*.pyc" "*.jar" "*.tar.gz" "*.tgz" "*.zip" "*.png" "*.jpg" "*.gif")
                projectile-globally-ignored-files))
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
         ("f" . projectile-find-file)
         ("d" . projectile-find-dir)
         ("b" . projectile-switch-to-buffer)
         ("p" . projectile-switch-project)
         ("s" . projectile-ripgrep)
         ("a" . projectile-find-other-file)))

;; Project-based directory tree with Treemacs
(use-package treemacs-projectile
  :after (treemacs projectile)
  :config
  (setq treemacs-position 'left
        treemacs-width 35))

;; Add project management keybindings to Evil
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "<leader>pp") 'projectile-switch-project
    (kbd "<leader>pf") 'projectile-find-file
    (kbd "<leader>pb") 'projectile-switch-to-buffer
    (kbd "<leader>pd") 'projectile-find-dir
    (kbd "<leader>ps") 'projectile-ripgrep
    (kbd "<leader>pt") 'treemacs-projectile)
  
  ;; Update which-key descriptions for project commands
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "SPC p" "project")))

(provide 'init.projectile)
