;; Enhanced Git integration with blame info and modeline indicators

;; Git basics with magit
(use-package magit
  :bind
  ("M-g s" . magit-status)       ; Open Magit status buffer
  ("M-g d" . magit-dispatch)     ; Open Magit dispatch popup
  ("M-g b" . magit-blame)        ; Quick git blame
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1) ; Show Magit buffers in same window
  :init
  (setq magit-save-repository-buffers 'dontask) ; Don't ask to save modified buffers when running Magit commands
  (setq magit-repository-directories '(("~/Projects/" . 1))) ; Set the default Magit repository search path
  (setq magit-auto-revert-mode t)) ; Auto-revert buffers when files change on disk

;; Git blame information in the modeline with blamer
(use-package blamer
  :bind (("M-g B" . blamer-mode))  ; Toggle permanent inline blame
  :custom
  (blamer-idle-time 0.3)           ; Wait before showing blame
  (blamer-min-offset 40)           ; Minimum offset from left side of window
  (blamer-author-formatter " ✎ %s ") ; Format author string
  (blamer-datetime-formatter "[%s]") ; Format datetime string
  (blamer-commit-formatter " • %s") ; Format commit message
  (blamer-max-commit-message-length 30) ; Maximum length of commit messages
  (blamer-self-author-name "You") ; Name to use for self-authored code
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :italic t
                   :height 0.9))))

;; Show current git info in the modeline
(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  :config
  (setq diff-hl-draw-borders nil))

;; Git Gutter - show git changes in the left margin
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 1)
  ;; These symbols work well with most fonts
  (setq git-gutter:modified-sign "≡"  ; TRIPLE BAR ≡
        git-gutter:added-sign "+"     ; PLUS +
        git-gutter:deleted-sign "-")) ; MINUS -

;; Add git info to modeline (to be used with mood-line)
(use-package minions
  :config
  (minions-mode 1))

;; Git timemachine for navigating through git history
(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

;; Browse remote repositories
(use-package browse-at-remote
  :bind ("M-g r" . browse-at-remote))

;; Show commit message at point
(use-package git-messenger
  :bind ("M-g m" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

;; Custom git-related functions

;; Function to show git blame for current line
(defun my/git-show-current-line-blame ()
  "Show git blame info for the current line."
  (interactive)
  (let* ((line-number (line-number-at-pos))
         (file-name (buffer-file-name))
         (git-output (shell-command-to-string
                      (format "git blame -L %d,%d %s"
                              line-number
                              line-number
                              file-name))))
    (when (string-match "^\\([^ ]+\\) \\([^)]+\\) (\\(.*\\)) \\(.*\\)" git-output)
      (let ((commit-hash (match-string 1 git-output))
            (author (match-string 3 git-output))
            (date (match-string 2 git-output))
            (text (match-string 4 git-output)))
        (message "Line %d: %s committed by %s on %s: %s"
                 line-number
                 (substring commit-hash 0 8)
                 author
                 date
                 text)))))

;; Bind the function
(global-set-key (kbd "M-g l") 'my/git-show-current-line-blame)


(provide 'init.git)
