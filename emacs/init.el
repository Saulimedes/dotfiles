(defconst emacs-cache-dir (expand-file-name "~/.cache/emacs/"))

(defconst my/elpa-dir (expand-file-name "elpa" emacs-cache-dir))
(unless (file-directory-p elpa-dir)
  (make-directory elpa-dir t))
(setq package-user-dir elpa-dir)

(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-log-done 'time)
  (setq org-hide-emphasis-markers t)
  (setq org-src-tab-acts-natively t))

(setq custom-file (expand-file-name "custom.el" emacs-cache-dir))
(when (file-exists-p custom-file)
  (load custom-file))
