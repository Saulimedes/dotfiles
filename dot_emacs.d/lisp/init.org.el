;; -*- lexical-binding: t; -*-
;; Enhanced Org Mode setup for productivity
(use-package f)
(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         (org-mode . (lambda () (when (executable-find "aspell") (flyspell-mode 1)))))
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

;; ============================================================
;; Org-roam - Zettelkasten / knowledge graph
;; ============================================================
(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :target (file+head "ref/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :reference:\n")
      :unnarrowed t)
     ("p" "project" plain "%?"
      :target (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :project:\n")
      :unnarrowed t)))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph)
         ("C-c n d" . org-roam-dailies-goto-today)
         ("C-c n D" . org-roam-dailies-goto-date)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

;; Flyspell for prose (comments in prog-mode)
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; ============================================================
;; Org-Pomodoro - Pomodoro technique for org tasks
;; ============================================================
(use-package org-pomodoro
  :after org
  :bind (:map org-mode-map
              ("C-c C-x p" . org-pomodoro))
  :custom
  (org-pomodoro-length 25)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-long-break-length 15)
  (org-pomodoro-long-break-frequency 4)
  (org-pomodoro-audio-player (or (executable-find "paplay")
                                 (executable-find "aplay")))
  (org-pomodoro-play-sounds t)
  (org-pomodoro-keep-killed-pomodoro-time t)
  :config
  ;; Show pomodoro in modeline
  (setq org-pomodoro-format " %s"))

(provide 'init.org)
