;; Project-specific workflows and configurations
;; Provides per-project settings, directory management, and org integration

;; Base project management with project.el
(use-package project
  :ensure nil  ;; Built into Emacs
  :custom
  (project-switch-commands 'project-find-file)
  :config
  ;; Store project-specific settings
  (defvar project-local-variables (make-hash-table :test 'equal)
    "Hash table for project-specific variables.")
  
  ;; Project-specific variables
  (defmacro defprojectvar (name &optional default docstring)
    "Define a project-specific variable NAME with DEFAULT value and DOCSTRING."
    `(progn
       (defun ,(intern (format "project-%s" name)) (&optional project)
         ,(or docstring (format "Get project-specific value for %s." name))
         (let ((project (or project (project-current))))
           (when project
             (gethash (cons ',name (project-root project)) project-local-variables ,default))))
       
       (defun ,(intern (format "set-project-%s" name)) (value &optional project)
         ,(or docstring (format "Set project-specific value for %s." name))
         (let ((project (or project (project-current))))
           (when project
             (puthash (cons ',name (project-root project)) value project-local-variables)
             value)))))
  
  ;; Define project-specific variables
  (defprojectvar org-file nil "Project-specific org file.")
  (defprojectvar notes-directory nil "Project-specific notes directory.")
  (defprojectvar compile-command nil "Project-specific compile command.")
  (defprojectvar test-command nil "Project-specific test command.")
  (defprojectvar run-command nil "Project-specific run command.")
  (defprojectvar env-file nil "Project-specific environment file.")
  (defprojectvar todo-tags nil "Project-specific TODO tags for org mode.")
  (defprojectvar known-directories nil "List of important project directories.")
  
  ;; Add command to edit project variables
  (defun project-edit-variables ()
    "Edit variables for the current project."
    (interactive)
    (let* ((project (project-current t))
           (project-root (project-root project))
           (buffer-name (format "*Project Variables: %s*" 
                                (file-name-nondirectory (directory-file-name project-root))))
           (var-names '(org-file notes-directory compile-command 
                                test-command run-command env-file 
                                todo-tags known-directories))
           (vars (mapcar (lambda (var)
                           (cons var (funcall (intern (format "project-%s" var)) project)))
                         var-names)))
      
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (special-mode)
        (let ((inhibit-read-only t))
          (insert (format "Project variables for %s\n\n" project-root))
          (dolist (var vars)
            (let ((name (car var))
                  (value (cdr var)))
              (insert (format "%s: %s\n" name value))
              (let ((new-value (read-string (format "Set %s (leave empty to keep): " name)
                                          "" nil nil t)))
                (unless (string-empty-p new-value)
                  (funcall (intern (format "set-project-%s" name)) 
                         (if (member name '(known-directories todo-tags))
                             (split-string new-value)
                           new-value)
                         project)))))
          (insert "\nVariables updated successfully.")
          (goto-char (point-min)))
        (switch-to-buffer buffer-name)))))

;; Enhanced projectile configuration for project management
(use-package projectile
  :after project
  :init
  (setq projectile-project-search-path '("~/Projects")
        projectile-sort-order 'recently-active
        projectile-enable-caching t
        projectile-completion-system 'default  ; Use Vertico/Marginalia
        projectile-indexing-method 'hybrid)
  
  :config
  (projectile-mode +1)
  
  ;; Function to create a new project
  (defun projectile-create-project (dir)
    "Create a new project in DIR."
    (interactive "DCreate project in directory: ")
    (unless (file-exists-p dir)
      (make-directory dir t))
    (let ((default-directory dir))
      ;; Create basic project structure
      (make-directory "src" t)
      (make-directory "docs" t)
      (make-directory "notes" t)
      
      ;; Create project org file
      (find-file (expand-file-name "project.org" dir))
      (insert "#+TITLE: " (file-name-nondirectory (directory-file-name dir)) " Project\n")
      (insert "#+AUTHOR: " user-full-name "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      (insert "* Project Overview\n\n")
      (insert "* Tasks [/]\n")
      (insert "* Notes\n")
      (insert "* Resources\n")
      (save-buffer)
      
      ;; Set project-specific variables
      (set-project-org-file (expand-file-name "project.org" dir) 
                          (project-current nil dir))
      (set-project-notes-directory (expand-file-name "notes" dir)
                                 (project-current nil dir)))
    
    (message "Project created in %s" dir))
  
  ;; Integrate with direnv for project-specific environments
  (use-package direnv
    :config
    (direnv-mode))
  
  ;; Create project command menu
  (defun projectile-project-command-menu ()
    "Show menu of project commands."
    (interactive)
    (let* ((project (projectile-ensure-project (projectile-project-root)))
           (compile-cmd (or (project-compile-command project) "make"))
           (test-cmd (or (project-test-command project) "make test"))
           (run-cmd (or (project-run-command project) "make run"))
           (command (completing-read 
                    "Project command: " 
                    `(("compile" . ,compile-cmd)
                     ("test" . ,test-cmd)
                     ("run" . ,run-cmd)
                     ("edit variables" . project-edit-variables)
                     ("open org file" . projectile-open-project-org-file)
                     ("new note" . projectile-create-project-note)))))
      (cond
       ((string= command "compile") 
        (compile compile-cmd))
       ((string= command "test") 
        (compile test-cmd))
       ((string= command "run") 
        (compile run-cmd))
       ((string= command "edit variables") 
        (project-edit-variables))
       ((string= command "open org file") 
        (projectile-open-project-org-file))
       ((string= command "new note") 
        (projectile-create-project-note))))))

;; Project-specific directory management
(defun projectile-known-directories ()
  "Show known directories for this project and jump to one."
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (known-dirs (project-known-directories project))
         (root (projectile-project-root))
         (dirs (if known-dirs
                  known-dirs
                (directory-files root t directory-files-no-dot-files-regexp))))
    (let ((dir (completing-read "Directory: " 
                              (mapcar (lambda (d) 
                                        (file-relative-name d root)) 
                                      dirs))))
      (dired (expand-file-name dir root)))))

;; Project-specific org-mode integration
(defun projectile-open-project-org-file ()
  "Open the org file for the current project."
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (org-file (project-org-file project)))
    (if (and org-file (file-exists-p org-file))
        (find-file org-file)
      (let* ((root (projectile-project-root))
             (default-file (expand-file-name "project.org" root)))
        (when (y-or-n-p (format "No project org file defined. Create one in %s? " default-file))
          (find-file default-file)
          (when (= (buffer-size) 0)
            (insert "#+TITLE: " (file-name-nondirectory (directory-file-name root)) " Project\n")
            (insert "#+AUTHOR: " user-full-name "\n")
            (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
            (insert "* Project Overview\n\n")
            (insert "* Tasks [/]\n")
            (insert "* Notes\n")
            (insert "* Resources\n")
            (save-buffer))
          (set-project-org-file default-file project))))))

;; Project-specific note taking
(defun projectile-create-project-note ()
  "Create a new note in the project notes directory."
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (notes-dir (project-notes-directory project)))
    (unless notes-dir
      (let ((root (projectile-project-root)))
        (setq notes-dir (expand-file-name "notes" root))
        (unless (file-exists-p notes-dir)
          (make-directory notes-dir t))
        (set-project-notes-directory notes-dir project)))
    
    (let* ((title (read-string "Note title: "))
           (slug (replace-regexp-in-string "[^a-zA-Z0-9]" "-" (downcase title)))
           (date-prefix (format-time-string "%Y%m%d-"))
           (filename (expand-file-name (concat date-prefix slug ".org") notes-dir)))
      (find-file filename)
      (when (= (buffer-size) 0)
        (insert "#+TITLE: " title "\n")
        (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n")
        (insert "#+AUTHOR: " user-full-name "\n")
        (insert "#+FILETAGS: :note:project:\n\n")
        (save-buffer)))))

;; Project-specific agenda view
(defun projectile-project-agenda ()
  "Show agenda for the current project."
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (org-file (project-org-file project)))
    (if (and org-file (file-exists-p org-file))
        (let ((org-agenda-files (list org-file)))
          (org-agenda nil "t"))
      (message "No project org file defined. Use M-x projectile-open-project-org-file to create one."))))

;; Org-mode capture template for project tasks
(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("p" "Project Task" entry
                 (file+headline (lambda () 
                                 (let ((project (project-current t)))
                                   (or (project-org-file project)
                                       (expand-file-name "project.org" (project-root project)))))
                              "Tasks")
                 "* TODO %?\n%i\n%a")))

;; Easy directory switching within a project
(defun projectile-find-project-dir ()
  "Find a directory within the current project."
  (interactive)
  (let* ((root (projectile-project-root))
         (dirs (projectile-project-dirs))
         (dir (completing-read "Project directory: " dirs)))
    (dired (expand-file-name dir root))))

;; Integration with Evil mode
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    ;; Project navigation and management
    (kbd "<leader>p") '(:ignore t :which-key "project")
    (kbd "<leader>pf") 'projectile-find-file
    (kbd "<leader>pd") 'projectile-find-project-dir
    (kbd "<leader>pb") 'projectile-switch-to-buffer
    (kbd "<leader>pp") 'projectile-switch-project
    (kbd "<leader>pk") 'projectile-kill-buffers
    
    ;; Project commands
    (kbd "<leader>pc") '(:ignore t :which-key "commands")
    (kbd "<leader>pcc") 'projectile-compile-project
    (kbd "<leader>pct") 'projectile-test-project
    (kbd "<leader>pcr") 'projectile-run-project
    (kbd "<leader>pcm") 'projectile-project-command-menu
    
    ;; Project org integration
    (kbd "<leader>po") '(:ignore t :which-key "org")
    (kbd "<leader>pof") 'projectile-open-project-org-file
    (kbd "<leader>pon") 'projectile-create-project-note
    (kbd "<leader>poa") 'projectile-project-agenda
    (kbd "<leader>pot") 'org-capture) ; Will use "p" template for project tasks
    
    ;; Project configuration
    (kbd "<leader>pv") 'project-edit-variables
    (kbd "<leader>pn") 'projectile-create-project)
  
  ;; Update which-key descriptions
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "SPC p" "project")
    (which-key-add-key-based-replacements "SPC pc" "commands")
    (which-key-add-key-based-replacements "SPC po" "org")))

;; Direnv for automatic environment setup per project
(use-package direnv
  :config
  (direnv-mode))

;; Per-project envrc files
(use-package envrc
  :config
  (envrc-global-mode))

;; Persist project-specific variables
(defun save-project-variables ()
  "Save project variables to a file."
  (interactive)
  (let ((file (expand-file-name "project-variables.el" user-emacs-directory)))
    (with-temp-file file
      (print project-local-variables (current-buffer)))
    (message "Project variables saved to %s" file)))

(defun load-project-variables ()
  "Load project variables from a file."
  (interactive)
  (let ((file (expand-file-name "project-variables.el" user-emacs-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (setq project-local-variables (read (current-buffer))))
      (message "Project variables loaded from %s" file))))

;; Load project variables on startup
(add-hook 'after-init-hook 'load-project-variables)
;; Save project variables when Emacs exits
(add-hook 'kill-emacs-hook 'save-project-variables)

(provide 'init.project-manager)