;; org
(use-package f)
(use-package org)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory "~/Documents/org")
(setq org-agendafiles '(concat org-directory "/agenda.org"))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-default-notes-file (concat org-directory "/notes.org"))

;;; org bullets
(use-package org-bullets
  :init
  (setq org-bullets-bullet-list
        '("🜁" "🜂" "🜃" "🜄" "🜅" "🜆"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init.org)
