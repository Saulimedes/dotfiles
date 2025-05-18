;; Set preset Size of the Frame 
(setq default-frame-alist
      '((width . 160)  ; Set the window width to 160 columns
        (height . 60)  ; Set the window height to 60 rows
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars . nil)))

;; Better frame behavior
(setq frame-title-format '("%b - Emacs")
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Font setup
(use-package fontaine
  :init
  (setq fontaine-presets
        '((regular
           :default-family "VictorMono Nerd Font Propo Medium"
           :default-height 120
           :variable-pitch-family "SF Pro"
           :variable-pitch-height 120
           :fixed-pitch-family "VictorMono Nerd Font Propo Medium"
           :fixed-pitch-height 120
           :line-spacing 1))))

(fontaine-set-preset 'regular)

(setq-default line-spacing 1)
(setq auto-composition-mode nil)

;; Theme setup with modus-themes (dark theme with true black background)
(use-package modus-themes
  :init
  ;; Configure modus-vivendi colors for true black background
  (setq modus-themes-mode-line '(accented borderless)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-fringes 'subtle
        modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)
        modus-themes-prompts '(bold intense)
        ;; Updated completions setting format
        modus-themes-completions '((matches . (extrabold))
                                  (selection . (semibold accented))
                                  (popup . (accented)))
        modus-themes-org-blocks 'greyscale
        modus-themes-scale-headings t
        modus-themes-region '(bg-only)
        ;; Make background pure black instead of dark grey
        modus-themes-common-palette-overrides
        '((bg-main "#000000")
          (bg-dim "#050505")
          (bg-alt "#121212")))
  :config
  (load-theme 'modus-vivendi t))

;; Alternative themes to try
(use-package catppuccin-theme
  :defer t)

(use-package ef-themes
  :defer t)

(use-package modus-themes
  :defer t)

;; Function to cycle between my favorite themes
(defun my/cycle-theme ()
  "Cycle between favorite themes."
  (interactive)
  (let ((themes '(doom-nord doom-dracula doom-one catppuccin-mocha ef-dark modus-vivendi))
        (current-theme (car custom-enabled-themes))
        (next-theme nil))
    (if (not current-theme)
        (load-theme (car themes) t)
      (setq next-theme
            (or (cadr (member current-theme themes))
                (car themes)))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme next-theme t))
    (message "Loaded theme: %s" (car custom-enabled-themes))))

;; Highlight matching parentheses with better color
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; pulsar - enhanced visual feedback
(use-package pulsar
  :init
  (setq pulsar-pulse t
        pulsar-delay 0.03
        pulsar-iterations 5
        pulsar-face 'pulsar-cyan)
  :config
  (pulsar-global-mode 1))

;; line numbers with better performance
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

;; Highlight indentation levels
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0.1))

;; Custom modeline with git branch and blame information
(use-package mood-line
  :config
  ;; Add git branch indicator to mood-line
  (defvar-local my/git-current-branch nil)
  (defvar-local my/git-last-blame-info nil)
  
  ;; Update git branch info
  (defun my/update-git-branch-info ()
    "Update the git branch information for the current buffer."
    (when (and buffer-file-name
               (file-exists-p buffer-file-name)
               (not (file-remote-p buffer-file-name)))
      (let* ((branch (magit-get-current-branch))
             (default-branch (or (magit-get "init.defaultBranch") "main")))
        (setq my/git-current-branch 
              (if (and branch (not (string= branch default-branch)) (not (string= branch "master")))
                  (format " ⎇ %s" branch)
                nil)))))
  
  ;; Update git blame for current line
  (defun my/update-git-blame-info ()
    "Update git blame info for current line."
    (when (and buffer-file-name
               (file-exists-p buffer-file-name)
               (not (file-remote-p buffer-file-name))
               (vc-git-root buffer-file-name))
      (let* ((line-number (line-number-at-pos (point)))
             (file-name (buffer-file-name))
             (git-cmd (format "git blame -L %d,%d --porcelain %s" 
                              line-number line-number file-name))
             (git-output (with-temp-buffer
                           (call-process-shell-command git-cmd nil t nil)
                           (buffer-string))))
        (if (string-match "^\\([^ ]+\\) \\([^(]*\\)(\\(.*\\)) \\([0-9]+\\) \\([0-9]+\\).*\n.*\nsummary \\(.*\\)" git-output)
            (let ((commit-hash (match-string 1 git-output))
                  (author (match-string 3 git-output))
                  (timestamp (match-string 2 git-output))
                  (summary (match-string 6 git-output)))
              (when (not (string= commit-hash "0000000000000000000000000000000000000000"))
                (setq my/git-last-blame-info
                      (format " ✎ %s: %s" author (truncate-string-to-width summary 25 nil nil "...")))))
          (setq my/git-last-blame-info nil)))))
  
  ;; Add git info segment to mood-line
  (defun my/mood-line-segment-git-info ()
    "Display git branch and blame info in the modeline."
    (let ((branch my/git-current-branch)
          (blame my/git-last-blame-info))
      (when (or branch blame)
        (concat
         (if branch branch "")
         (if blame blame "")))))
  
  ;; Add idle timer to update blame info
  (defvar my/git-blame-timer nil)
  (defun my/setup-git-blame-timer ()
    "Set up timer to update git blame info."
    (when my/git-blame-timer
      (cancel-timer my/git-blame-timer))
    (setq my/git-blame-timer
          (run-with-idle-timer 0.5 t #'my/update-git-blame-info)))
  
  ;; Add hooks to update git info
  (add-hook 'find-file-hook #'my/update-git-branch-info)
  (add-hook 'after-save-hook #'my/update-git-branch-info)
  (add-hook 'magit-refresh-buffer-hook #'my/update-git-branch-info)
  (add-hook 'find-file-hook #'my/setup-git-blame-timer)
  
  ;; Customize mood-line-segments-left and mood-line-segments-right to include the git info
  ;; Redefine mood-line functions to include our git info
  (defun mood-line--update-segment-forms ()
    "Update the segment forms based on the segment functions."
    (setq mood-line--segment-forms
          (list :left `(,@(mapcar #'funcall mood-line-segments-left)
                              ,(my/mood-line-segment-git-info))
                :right (mapcar #'funcall mood-line-segments-right))))
  
  ;; Activate mood-line
  (mood-line-mode)
  
  ;; Customize mood-line
  (setq mood-line-show-encoding-information nil  ; Don't show encoding
        mood-line-show-eol-style nil             ; Don't show EOL style
        mood-line-show-cursor-point t            ; Show cursor position
        mood-line-show-indentation-style t))     ; Show indentation style

;; Alternative: Even more minimal mode line
(use-package mini-modeline
  :defer t
  :config
  (mini-modeline-mode t)
  (setq mini-modeline-r-format
        '("%e"
          mode-line-front-space
          (:eval (if (doom-modeline--active)
                     (concat " " (doom-modeline-buffer-file-name))
                   (concat " " (buffer-name))))
          " "
          (:eval (mood-line-segment-modified))
          " "
          (:eval (mood-line-segment-position))
          " "
          (:eval (mood-line-segment-major-mode))
          " "
          (:eval (mood-line-segment-process))
          mode-line-end-spaces)))

;; ligatures with better setup
(use-package ligature
  :config
  ;; Enable ligatures in all modes
  (global-ligature-mode t)
  ;; Use the following ligatures
  (ligature-set-ligatures 't '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                             ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                             "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                             "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                             "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                             "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                             "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                             "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                             ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                             "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                             "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                             "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                             "\\\\" "://")))

;; mode-line


;; all the icons
(use-package all-the-icons
  :if (display-graphic-p))

;; page-break-lines-mode
(use-package page-break-lines
  :if (display-graphic-p))

;; Dashboard
(use-package dashboard
  :after all-the-icons page-break-lines
  :custom
  ;; Set the banner
  (dashboard-startup-banner
   (if (display-graphic-p)
       (expand-file-name "dashboard/banner.png" user-emacs-directory)
     (expand-file-name "dashboard/banner.txt" user-emacs-directory)))
  ;; Dashboard layout/appearance
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-banner-logo-title "Welcome to Emacs")
  (dashboard-set-init-info nil)
  (dashboard-set-file-icons t)
  (dashboard-footer-icon "")
  (dashboard-footer-messages '("Don't try to solve serious matters in the middle of the night."))
  ;; Content settings
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5)))
  ;; Navigate with vi keys
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
        "GitHub" "Browse GitHub"
        (lambda (&rest _) (browse-url "https://github.com")))
      (,(all-the-icons-fileicon "emacs" :height 1.1 :v-adjust 0.0)
        "Config" "Open config folder"
        (lambda (&rest _) (find-file user-emacs-directory))))))
  ;; Make sure dashboard works with emacsclient
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook 'page-break-lines-mode))

;; A simpler function to jump to dashboard sections
(defun my/dashboard-goto-section (section)
  "Jump to the specified SECTION in the dashboard."
  (interactive "sSection (recent, bookmarks, projects, agenda): ")
  (when (get-buffer "*dashboard*")
    (with-current-buffer "*dashboard*"
      (goto-char (point-min))
      (let ((section-regexp (pcase section
                             ("recent" "Recent Files:")
                             ("bookmarks" "Bookmarks:")
                             ("projects" "Projects:")
                             ("agenda" "Agenda:")
                             (_ "Recent Files:"))))
        (when (re-search-forward section-regexp nil t)
          (forward-line 1)
          (beginning-of-line))))))

;; Remove the problematic hook that's causing the timer error
;; We'll use a simpler approach for dashboard navigation

;; Add a simple hook to ensure we can navigate the dashboard easily
(defun my/dashboard-setup ()
  "Set up dashboard with better defaults."
  (when (get-buffer "*dashboard*")
    (with-current-buffer "*dashboard*"
      ;; Make sure we start at the beginning of the buffer
      (goto-char (point-min))
      ;; Add a small hint at the top for navigation
      (setq-local header-line-format 
                  " Dashboard: use n/p to navigate, RET to select"))))

;; Add the simpler hook function to dashboard-mode-hook
(add-hook 'dashboard-mode-hook #'my/dashboard-setup)

;; Define convenient keybinding for dashboard navigation
(with-eval-after-load 'dashboard
  ;; Add keybindings in dashboard mode (god-mode friendly)
  (define-key dashboard-mode-map (kbd "n") 'dashboard-next-line)
  (define-key dashboard-mode-map (kbd "p") 'dashboard-previous-line)
  (define-key dashboard-mode-map (kbd "C-n") 'dashboard-next-section)
  (define-key dashboard-mode-map (kbd "C-p") 'dashboard-previous-section)
  (define-key dashboard-mode-map (kbd "RET") 'dashboard-return)
  (define-key dashboard-mode-map [return] 'dashboard-return)
  (define-key dashboard-mode-map [down-mouse-1] 'dashboard-mouse-1)
  
  ;; Add keybindings for quick section navigation
  (define-key dashboard-mode-map (kbd "r") (lambda () (interactive) (my/dashboard-goto-section "recent")))
  (define-key dashboard-mode-map (kbd "b") (lambda () (interactive) (my/dashboard-goto-section "bookmarks")))
  (define-key dashboard-mode-map (kbd "j") (lambda () (interactive) (my/dashboard-goto-section "projects")))
  (define-key dashboard-mode-map (kbd "a") (lambda () (interactive) (my/dashboard-goto-section "agenda")))
  
  ;; Add a header with key help
  (setq dashboard-footer "Keys: [r]ecent [b]ookmarks pro[j]ects [a]genda [n/p] Navigate [RET] Select"))

(provide 'init.appearance)
