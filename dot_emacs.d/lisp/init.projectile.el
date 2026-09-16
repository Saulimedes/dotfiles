;; -*- lexical-binding: t; -*-
;; Enhanced project management with Projectile
;; Single canonical projectile config — do not add another `use-package
;; projectile` block elsewhere; project-specific helpers that need projectile
;; live in init.project-manager.el behind `with-eval-after-load 'projectile'.
(use-package projectile
  :diminish projectile-mode
  ;; :demand — several other modules (dashboard, tab-line project grouping,
  ;; devops menus) call projectile functions or check `projectile-mode' from
  ;; startup, so it must not be deferred behind the :bind-keymap below.
  :demand t
  :init
  (setq projectile-project-search-path '("~/Projects")
        projectile-switch-project-action #'projectile-find-file
        projectile-enable-caching t
        projectile-completion-system 'default ; Use Vertico/Marginalia instead of Ivy
        projectile-indexing-method 'alien
        projectile-sort-order 'recently-active)
  :config
  ;; These append to projectile's own default-value lists, so they must run
  ;; in :config (after projectile.el is loaded and its defcustoms exist) —
  ;; not :init, where the variables would still be void.
  (setq projectile-globally-ignored-directories
        (append '(".git" ".idea" ".vscode" "node_modules" "build" "dist" "target")
                projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files
        (append '("*.gz" "*.pyc" "*.jar" "*.tar.gz" "*.tgz" "*.zip" "*.png" "*.jpg" "*.gif")
                projectile-globally-ignored-files))
  (projectile-mode +1)

  ;; Create a new project scaffold (also called from the dashboard "n" key)
  (defun projectile-create-project (dir)
    "Create a new project in DIR."
    (interactive "DCreate project in directory: ")
    (unless (file-exists-p dir)
      (make-directory dir t))
    (let ((default-directory dir))
      (make-directory "src" t)
      (make-directory "docs" t)
      (make-directory "notes" t)
      (find-file (expand-file-name "project.org" dir))
      (insert "#+TITLE: " (file-name-nondirectory (directory-file-name dir)) " Project\n")
      (insert "#+AUTHOR: " user-full-name "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      (insert "* Project Overview\n\n")
      (insert "* Tasks [/]\n")
      (insert "* Notes\n")
      (insert "* Resources\n")
      (save-buffer)
      (set-project-org-file (expand-file-name "project.org" dir)
                             (project-current nil dir))
      (set-project-notes-directory (expand-file-name "notes" dir)
                                    (project-current nil dir)))
    (message "Project created in %s" dir))

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

(provide 'init.projectile)
