;; Enhanced Org Mode setup for productivity
(use-package f)
(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         (org-mode . flyspell-mode))
  :custom
  (org-directory "~/Documents/org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-agenda-files (list (concat org-directory "/agenda.org")
                         (concat org-directory "/work.org")
                         (concat org-directory "/personal.org")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  :config
  ;; Better org capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (concat org-directory "/agenda.org") "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Note" entry (file+headline (concat org-directory "/notes.org") "Notes")
           "* %?\n  %i\n  %a")
          ("i" "Idea" entry (file+headline (concat org-directory "/ideas.org") "Ideas")
           "* %?\n  %i\n  %a")))
  
  ;; Set org todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(p)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
  
  ;; Add colors to todo items
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "firebrick2" :weight bold))
          ("NEXT" . (:foreground "OrangeRed" :weight bold))
          ("IN-PROGRESS" . (:foreground "orange" :weight bold))
          ("WAITING" . (:foreground "RoyalBlue" :weight bold))
          ("SOMEDAY" . (:foreground "gray60" :weight bold))
          ("DONE" . (:foreground "forest green" :weight bold))
          ("CANCELLED" . (:foreground "gray50" :weight bold)))))

;; Pretty headings and bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list
   '("❁" "◉" "✿" "✱" "❂" "❖" "☯")))

;; Better looking org mode
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("●" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  (org-modern-checkbox '((?X . "☑") (?- . "◫") (?\  . "☐")))
  (org-modern-priority '((?A . "❗") (?B . "⬆") (?C . "⬇")))
  (org-modern-tag '("🏷" . "🏷"))
  (org-modern-block-fringe t)
  (org-modern-block-name '("" . " ")))

;; Supercharge agenda view
(use-package org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:name "Important"
                 :priority "A")
          (:name "Next Items"
                 :todo "NEXT")
          (:name "In Progress"
                 :todo "IN-PROGRESS")
          (:name "Waiting"
                 :todo "WAITING")
          (:name "Due Today"
                 :deadline today)
          (:name "Overdue"
                 :deadline past)
          (:name "Due Soon"
                 :deadline future)
          (:name "Someday"
                 :todo "SOMEDAY")
          (:name "Scheduled"
                 :scheduled future)))
  (org-super-agenda-mode))

;; Org-mode keybindings (Meow compatible)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o b") 'org-switchb)

;; Flyspell for prose (comments in prog-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'init.org)
