;; projectile
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Projects"))
  (setq projectile-switch-project-action #'projectile-dired))
(use-package counsel-projectile
  :after projectile
  :bind
  ("C-c p s" . counsel-projectile-switch-project)
  :config
  (counsel-projectile-mode))

(provide 'init.projectile)
