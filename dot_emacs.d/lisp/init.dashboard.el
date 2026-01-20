;; -*- lexical-binding: t; -*-
;; Modern dashboard with project workflow integration
;; Fast, functional, project-centric

(defgroup my/dashboard nil
  "Dashboard settings."
  :group 'convenience)

(defvar my/dashboard-buffer-name "*Dashboard*"
  "Name of the dashboard buffer.")

;; ============================================================
;; Faces
;; ============================================================
(defface my/dashboard-title
  '((t :inherit font-lock-keyword-face :height 1.8 :weight bold))
  "Face for dashboard title.")

(defface my/dashboard-subtitle
  '((t :inherit font-lock-comment-face :height 1.1))
  "Face for dashboard subtitle.")

(defface my/dashboard-heading
  '((t :inherit font-lock-type-face :height 1.15 :weight bold))
  "Face for dashboard section headings.")

(defface my/dashboard-item
  '((t :inherit default))
  "Face for dashboard items.")

(defface my/dashboard-shortcut
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for dashboard shortcuts.")

(defface my/dashboard-path
  '((t :inherit shadow :height 0.9))
  "Face for file paths.")

(defface my/dashboard-project-action
  '((t :inherit font-lock-function-name-face))
  "Face for project actions.")

(defvar my/dashboard-items
  '((projects . 6)
    (recents . 6)
    (agenda . 4))
  "Items to show on dashboard with counts.")

;; ============================================================
;; Helper functions
;; ============================================================

(defun my/dashboard--insert-centered (text)
  "Insert TEXT centered in the window."
  (let ((width (window-width)))
    (insert (make-string (max 0 (/ (- width (length text)) 2)) ?\s))
    (insert text)))

(defun my/dashboard--section-icon (section)
  "Return icon for SECTION if nerd-icons available."
  (if (fboundp 'nerd-icons-mdicon)
      (pcase section
        ('recents (nerd-icons-mdicon "nf-md-file_document_multiple"))
        ('projects (nerd-icons-mdicon "nf-md-folder_multiple"))
        ('agenda (nerd-icons-mdicon "nf-md-calendar_check"))
        ('bookmarks (nerd-icons-mdicon "nf-md-bookmark_multiple"))
        (_ ""))
    ""))

(defun my/dashboard--file-icon (file)
  "Return icon for FILE if nerd-icons available."
  (if (fboundp 'nerd-icons-icon-for-file)
      (concat (nerd-icons-icon-for-file file) " ")
    ""))

;; ============================================================
;; Section renderers
;; ============================================================

(defun my/dashboard--insert-recents (count)
  "Insert COUNT recent files."
  (require 'recentf)
  (recentf-mode 1)
  (when recentf-list
    (let ((files (seq-take (seq-filter #'file-exists-p recentf-list) count))
          (idx 0))
      (dolist (file files)
        (let* ((short-name (abbreviate-file-name file))
               (display-name (if (> (length short-name) 60)
                                 (concat "..." (substring short-name -57))
                               short-name))
               (key (if (< idx 9) (number-to-string (1+ idx)) nil)))
          (insert "    ")
          (when key
            (insert (propertize (format "[%s] " key) 'face 'my/dashboard-shortcut))
            ;; Bind number key to open this file
            (local-set-key (kbd key)
                           (let ((f file)) (lambda () (interactive) (find-file f)))))
          (insert (my/dashboard--file-icon file))
          (insert-text-button display-name
                              'action (lambda (_) (find-file file))
                              'follow-link t
                              'face 'my/dashboard-item
                              'help-echo file)
          (insert "\n")
          (setq idx (1+ idx)))))))

(defun my/dashboard--insert-projects (count)
  "Insert COUNT recent projects with quick actions."
  (let ((projects (cond
                   ;; Prefer projectile if available
                   ((and (fboundp 'projectile-relevant-known-projects)
                         (bound-and-true-p projectile-mode))
                    (seq-take (projectile-relevant-known-projects) count))
                   ;; Fallback to project.el
                   ((fboundp 'project-known-project-roots)
                    (seq-take (project-known-project-roots) count))
                   (t nil))))
    (if (not projects)
        (progn
          (insert "    ")
          (insert (propertize "No projects yet. Press 'n' to create one." 'face 'shadow))
          (insert "\n"))
      (dolist (proj projects)
        (let* ((name (file-name-nondirectory (directory-file-name proj)))
               (short-path (abbreviate-file-name proj))
               (has-envrc (file-exists-p (expand-file-name ".envrc" proj)))
               (has-flake (file-exists-p (expand-file-name "flake.nix" proj)))
               (has-git (file-exists-p (expand-file-name ".git" proj))))
          (insert "    ")
          ;; Project icon based on type
          (insert (if (fboundp 'nerd-icons-mdicon)
                      (concat (cond
                               (has-flake (nerd-icons-mdicon "nf-md-nix"))
                               (has-git (nerd-icons-mdicon "nf-md-git"))
                               (t (nerd-icons-mdicon "nf-md-folder")))
                              " ")
                    ""))
          ;; Project name button
          (insert-text-button name
                              'action (lambda (_)
                                        (let ((default-directory proj))
                                          (if (fboundp 'projectile-switch-project-by-name)
                                              (projectile-switch-project-by-name proj)
                                            (project-find-file))))
                              'follow-link t
                              'face 'my/dashboard-item
                              'help-echo proj)
          ;; Badges for project features
          (when has-envrc
            (insert (propertize " [env]" 'face 'font-lock-string-face)))
          (when has-flake
            (insert (propertize " [nix]" 'face 'font-lock-builtin-face)))
          ;; Path
          (insert (propertize (format "  %s" short-path) 'face 'my/dashboard-path))
          (insert "\n"))))))

(defun my/dashboard--insert-agenda (count)
  "Insert COUNT agenda items."
  (when (and (fboundp 'org-agenda-get-day-entries)
             (boundp 'org-agenda-files)
             org-agenda-files)
    (require 'org-agenda)
    (let* ((today (calendar-current-date))
           (entries (ignore-errors
                      (org-agenda-get-day-entries
                       (org-agenda-files nil 'ifmode)
                       today))))
      (if entries
          (let ((items (seq-take entries count)))
            (dolist (entry items)
              (let ((txt (get-text-property 0 'txt entry)))
                (insert "    ")
                (insert (if (fboundp 'nerd-icons-mdicon)
                            (concat (nerd-icons-mdicon "nf-md-checkbox_blank_outline") " ")
                          "- "))
                (insert (propertize (or txt "Unknown") 'face 'my/dashboard-item))
                (insert "\n"))))
        (insert "    ")
        (insert (propertize "No agenda items for today" 'face 'shadow))
        (insert "\n")))))

;; ============================================================
;; Main dashboard
;; ============================================================

(defun my/dashboard--render ()
  "Render the dashboard content."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Header with ASCII art
    (insert "\n")
    (my/dashboard--insert-centered
     (propertize "┌─────────────────────────────────┐" 'face 'my/dashboard-subtitle))
    (insert "\n")
    (my/dashboard--insert-centered
     (propertize "│         E M A C S               │" 'face 'my/dashboard-title))
    (insert "\n")
    (my/dashboard--insert-centered
     (propertize "└─────────────────────────────────┘" 'face 'my/dashboard-subtitle))
    (insert "\n\n")

    ;; Date and load time
    (my/dashboard--insert-centered
     (propertize (format-time-string "%A, %B %d %Y") 'face 'my/dashboard-subtitle))
    (insert "\n")
    (my/dashboard--insert-centered
     (propertize (format "Ready in %.2fs • %d packages"
                         (float-time (time-subtract after-init-time before-init-time))
                         (length package-activated-list))
                 'face 'shadow))
    (insert "\n\n")

    ;; Sections
    (dolist (item my/dashboard-items)
      (let ((section (car item))
            (count (cdr item)))
        (insert "  ")
        (insert (my/dashboard--section-icon section))
        (insert " ")
        (insert (propertize (capitalize (symbol-name section)) 'face 'my/dashboard-heading))
        (insert "\n")
        (pcase section
          ('recents (my/dashboard--insert-recents count))
          ('projects (my/dashboard--insert-projects count))
          ('agenda (my/dashboard--insert-agenda count))
          ('bookmarks (my/dashboard--insert-bookmarks count)))
        (insert "\n")))

    ;; Quick actions - organized in two rows
    (insert "  ")
    (insert (propertize "Actions" 'face 'my/dashboard-heading))
    (insert "\n")
    ;; Row 1: File/Project actions
    (insert "    ")
    (insert (propertize "[f]" 'face 'my/dashboard-shortcut))
    (insert " Find file  ")
    (insert (propertize "[p]" 'face 'my/dashboard-shortcut))
    (insert " Switch project  ")
    (insert (propertize "[n]" 'face 'my/dashboard-shortcut))
    (insert " New project  ")
    (insert (propertize "[r]" 'face 'my/dashboard-shortcut))
    (insert " Recent")
    (insert "\n")
    ;; Row 2: Workflow actions
    (insert "    ")
    (insert (propertize "[a]" 'face 'my/dashboard-shortcut))
    (insert " Agenda  ")
    (insert (propertize "[c]" 'face 'my/dashboard-shortcut))
    (insert " Capture  ")
    (insert (propertize "[t]" 'face 'my/dashboard-shortcut))
    (insert " Terminal  ")
    (insert (propertize "[m]" 'face 'my/dashboard-shortcut))
    (insert " Magit")
    (insert "\n")
    ;; Row 3: System actions
    (insert "    ")
    (insert (propertize "[s]" 'face 'my/dashboard-shortcut))
    (insert " Settings  ")
    (insert (propertize "[g]" 'face 'my/dashboard-shortcut))
    (insert " Refresh  ")
    (insert (propertize "[q]" 'face 'my/dashboard-shortcut))
    (insert " Quit")
    (insert "\n")

    ;; Move to first project
    (goto-char (point-min))
    (forward-line 8)))

(defun my/dashboard--setup-keys ()
  "Set up dashboard keybindings."
  ;; File actions
  (local-set-key (kbd "f") #'find-file)
  (local-set-key (kbd "r") #'consult-recent-file)

  ;; Project actions
  (local-set-key (kbd "p") (lambda () (interactive)
                             (if (fboundp 'projectile-switch-project)
                                 (projectile-switch-project)
                               (project-switch-project))))
  (local-set-key (kbd "n") (lambda () (interactive)
                             (if (fboundp 'projectile-create-project)
                                 (call-interactively 'projectile-create-project)
                               (message "projectile-create-project not available"))))

  ;; Workflow actions
  (local-set-key (kbd "a") #'org-agenda)
  (local-set-key (kbd "c") #'org-capture)
  (local-set-key (kbd "t") (lambda () (interactive)
                             (if (fboundp 'vterm)
                                 (vterm)
                               (eshell))))
  (local-set-key (kbd "m") #'magit-status)

  ;; System actions
  (local-set-key (kbd "s") (lambda () (interactive)
                             (find-file (expand-file-name "~/.emacs.d/init.el"))))
  (local-set-key (kbd "g") #'my/dashboard-refresh)
  (local-set-key (kbd "q") #'save-buffers-kill-terminal)

  ;; Navigation (vim-style)
  (local-set-key (kbd "j") #'forward-button)
  (local-set-key (kbd "k") #'backward-button)
  (local-set-key (kbd "RET") #'push-button)
  (local-set-key (kbd "TAB") #'forward-button)
  (local-set-key (kbd "<backtab>") #'backward-button)

  ;; Meow/motion mode overrides
  (local-set-key (kbd "h") #'backward-char)
  (local-set-key (kbd "l") #'forward-char))

(define-derived-mode my/dashboard-mode special-mode "Dashboard"
  "Major mode for the dashboard."
  (setq buffer-read-only t
        cursor-type nil
        truncate-lines t)
  (my/dashboard--setup-keys)
  ;; Integrate with meow/evil if present
  (when (fboundp 'meow-motion-mode) (meow-motion-mode 1)))

(defun my/dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (when (get-buffer my/dashboard-buffer-name)
    (with-current-buffer my/dashboard-buffer-name
      (my/dashboard--render))))

(defun my/create-dashboard ()
  "Create and return the dashboard buffer."
  (let ((buffer (get-buffer-create my/dashboard-buffer-name)))
    (with-current-buffer buffer
      (my/dashboard-mode)
      (my/dashboard--render))
    buffer))

(defun my/show-dashboard ()
  "Show the dashboard."
  (interactive)
  (switch-to-buffer (my/create-dashboard)))

;; ============================================================
;; Startup integration
;; ============================================================

(defvar my/dashboard-shown nil
  "Track if dashboard has been shown.")

(defun my/dashboard-init ()
  "Initialize dashboard on startup."
  (when (and (not my/dashboard-shown)
             (< (length command-line-args) 2))
    (setq my/dashboard-shown t)
    (my/show-dashboard)))

(add-hook 'emacs-startup-hook #'my/dashboard-init)

;; Global access
(global-set-key (kbd "C-c d") #'my/show-dashboard)

(provide 'init.dashboard)
