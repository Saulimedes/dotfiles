;; Simple startup dashboard
(defun my/create-dashboard ()
  "Create a simple dashboard."
  (let ((buffer (get-buffer-create "*Dashboard*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (insert "\n")
      (insert "    ███████╗███╗   ███╗ █████╗  ██████╗███████╗\n")
      (insert "    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝\n")
      (insert "    █████╗  ██╔████╔██║███████║██║     ███████╗\n")
      (insert "    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║\n")
      (insert "    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║\n")
      (insert "    ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝\n")
      (insert "\n")
      (center-line)
      (insert "                Welcome back, Paul!\n\n")
      (center-line)
      
      ;; Recent files
      (when (and (boundp 'recentf-list) recentf-list)
        (insert "Recent Files:\n")
        (dolist (file (seq-take recentf-list 5))
          (insert (format "  • %s\n" (file-name-nondirectory file))))
        (insert "\n"))
      
      ;; Quick actions
      (insert "Quick Actions:\n")
      (insert "  [f] Find file\n")
      (insert "  [p] Open project\n") 
      (insert "  [r] Recent files\n")
      (insert "  [s] Settings\n")
      (insert "  [q] Quit\n\n")
      
      (insert "Press SPC to open command palette\n")
      
      ;; Set up dashboard mode
      (local-set-key (kbd "f") (lambda () (interactive) (find-file "")))
      (local-set-key (kbd "p") #'project-switch-project)
      (local-set-key (kbd "r") #'recentf-open-files)
      (local-set-key (kbd "s") (lambda () (interactive) (find-file user-init-file)))
      (local-set-key (kbd "q") #'save-buffers-kill-terminal)
      (local-set-key (kbd "SPC") #'execute-extended-command)
      
      ;; Position cursor at first action item
      (goto-char (point-min))
      (when (search-forward "[f]" nil t)
        (beginning-of-line)
        (forward-char 2)) ; Position after the bracket for better visibility
      
      ;; Set Evil mode state if available
      (when (and (featurep 'evil) (fboundp 'evil-set-initial-state))
        (evil-set-initial-state 'fundamental-mode 'normal)))
    buffer))

;; Function to show dashboard
(defun my/show-dashboard ()
  "Show the dashboard."
  (interactive)
  (switch-to-buffer (my/create-dashboard)))

;; Track whether dashboard has been shown to prevent multiple triggers
(defvar my/dashboard-shown nil "Track if dashboard has been shown.")

;; Auto-show dashboard on startup - more reliable approach  
(add-hook 'emacs-startup-hook 
          (lambda ()
            ;; Only show dashboard if we started with no file arguments and haven't shown it yet
            (when (and (not my/dashboard-shown)
                       (not (buffer-file-name))
                       (or (string= (buffer-name) "*scratch*")
                           (string= (buffer-name) "*Messages*")))
              (setq my/dashboard-shown t)
              (run-with-timer 0.1 nil #'my/show-dashboard))))

;; Also make dashboard easily accessible
(global-set-key (kbd "C-c d") #'my/show-dashboard)

(provide 'init.dashboard)