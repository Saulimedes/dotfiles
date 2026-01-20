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
  
  ;; Direnv integration is handled separately in envrc section below
  
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

;; ============================================================
;; Direnv + Nix integration
;; ============================================================
;; envrc is preferred over direnv.el for better integration
(use-package envrc
  :diminish envrc-mode
  :init
  (setq envrc-show-summary-in-minibuffer t)
  :config
  (envrc-global-mode)
  ;; Keybindings for envrc management
  :bind-keymap ("C-c E" . envrc-command-map))

;; ============================================================
;; Nix-mode enhancements for project development
;; ============================================================
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . (lambda ()
                      ;; Use nixd for LSP
                      (when (fboundp 'eglot-ensure)
                        (eglot-ensure))))
  :config
  ;; Nix formatting with nixfmt
  (setq nix-nixfmt-bin "nixfmt"))

;; ============================================================
;; Nix project templates for .envrc
;; ============================================================
(defvar my/nix-envrc-templates
  '(("flake" . "use flake")
    ("flake-impure" . "use flake --impure")
    ("nix-shell" . "use nix")
    ("devenv" . "eval \"$(devenv shell-bash 2>/dev/null)\"")
    ("lorri" . "eval \"$(lorri direnv)\""))
  "Templates for .envrc files in Nix projects.")

(defun my/create-project-envrc ()
  "Create or update .envrc file for current project with Nix integration."
  (interactive)
  (let* ((root (or (projectile-project-root) (project-root (project-current t))))
         (envrc-path (expand-file-name ".envrc" root))
         (has-flake (file-exists-p (expand-file-name "flake.nix" root)))
         (has-shell-nix (file-exists-p (expand-file-name "shell.nix" root)))
         (has-default-nix (file-exists-p (expand-file-name "default.nix" root)))
         (template-names (mapcar #'car my/nix-envrc-templates))
         (recommended (cond
                       (has-flake "flake")
                       ((or has-shell-nix has-default-nix) "nix-shell")
                       (t "flake")))
         (choice (completing-read
                  (format "Envrc template (recommended: %s): " recommended)
                  template-names nil t nil nil recommended))
         (template (cdr (assoc choice my/nix-envrc-templates))))
    (with-temp-file envrc-path
      (insert "# Auto-generated .envrc for Nix project\n")
      (insert (format "# Template: %s\n\n" choice))
      (insert template)
      (insert "\n"))
    (message "Created %s with template: %s" envrc-path choice)
    ;; Reload envrc
    (when (fboundp 'envrc-reload)
      (envrc-reload))))

;; ============================================================
;; Project creation with Nix support
;; ============================================================
(defvar my/nix-flake-template
  "
{
  description = \"%s - A Nix flake project\";

  inputs = {
    nixpkgs.url = \"github:NixOS/nixpkgs/nixos-unstable\";
    flake-utils.url = \"github:numtide/flake-utils\";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Add your development dependencies here
          ];

          shellHook = ''
            echo \"Entering %s development environment\"
          '';
        };
      });
}
"
  "Template for flake.nix files.")

(defun my/create-nix-project (dir)
  "Create a new Nix-based project in DIR with flake and envrc."
  (interactive "DCreate Nix project in: ")
  (unless (file-exists-p dir)
    (make-directory dir t))
  (let* ((name (file-name-nondirectory (directory-file-name dir)))
         (flake-path (expand-file-name "flake.nix" dir))
         (envrc-path (expand-file-name ".envrc" dir))
         (gitignore-path (expand-file-name ".gitignore" dir)))
    ;; Create flake.nix
    (unless (file-exists-p flake-path)
      (with-temp-file flake-path
        (insert (format my/nix-flake-template name name))))
    ;; Create .envrc
    (unless (file-exists-p envrc-path)
      (with-temp-file envrc-path
        (insert "use flake\n")))
    ;; Create .gitignore
    (unless (file-exists-p gitignore-path)
      (with-temp-file gitignore-path
        (insert "# Nix\n")
        (insert "result\n")
        (insert "result-*\n")
        (insert "\n# direnv\n")
        (insert ".direnv/\n")))
    ;; Initialize git if not already
    (let ((default-directory dir))
      (unless (file-exists-p (expand-file-name ".git" dir))
        (shell-command "git init")))
    ;; Add to projectile
    (when (fboundp 'projectile-add-known-project)
      (projectile-add-known-project dir))
    (message "Created Nix project in %s" dir)
    (find-file flake-path)))

;; Add to project command menu
(with-eval-after-load 'projectile
  (defun projectile-create-envrc ()
    "Create .envrc for current project."
    (interactive)
    (my/create-project-envrc))

  ;; Bind to projectile map
  (define-key projectile-command-map (kbd "E") 'projectile-create-envrc))

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