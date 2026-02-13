;; -*- lexical-binding: t; -*-
;; sidebar
(use-package dired-sidebar
  :bind ("C-c s" . dired-sidebar-toggle-sidebar))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package treemacs
  :bind ("C-c S" . treemacs))

(provide 'init.dired)
