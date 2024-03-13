;; sidebar
(use-package dired-sidebar
  :bind ("C-c s" . dired-sidebar-toggle-sidebar))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :config (all-the-icons-ivy-rich-mode 1))

(use-package treemacs
  :bind ("C-c S" . treemacs))

(provide 'init.dired)
