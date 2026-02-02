;; Enhanced Eshell configuration to make it similar to Fish shell

(use-package eshell
  :ensure nil  ;; Built into Emacs
  :commands (eshell eshell-command)
  :init
  ;; Basic eshell settings
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-banner-message ""     ;; No banner on startup
        eshell-destroy-buffer-when-process-dies t)

  :config
  ;; Create directory for eshell files
  (defvar eshell-directory-name (expand-file-name "eshell" emacs-cache-directory))
  (make-directory eshell-directory-name t)
  (setq eshell-history-file-name (expand-file-name "history" eshell-directory-name)
        eshell-last-dir-ring-file-name (expand-file-name "lastdir" eshell-directory-name)
        eshell-aliases-file (expand-file-name "aliases" eshell-directory-name))

  ;; Better visual commands
  (setq eshell-visual-commands
        '("less" "more" "top" "htop" "vim" "vi" "nvim" "ncmpcpp" "nano"
          "dstat" "watch" "tail" "ssh" "zsh" "bash" "nmtui" "alsamixer" "ranger"
          "k9s" "lazygit" "lazydocker"))

  ;; Configure eshell prompt
  (setq eshell-prompt-regexp "^[^❯#$\n]*[❯#$] ")

  ;; Define a pretty prompt with git info (like Fish/Starship)
  (defun eshell-prompt-function ()
    (let ((current-branch (magit-get-current-branch)))
      (concat
       (propertize (abbreviate-file-name (eshell/pwd))
                   'face '(:foreground "#8FBCBB" :weight bold))
       (when current-branch
         (propertize (format " (%s)" current-branch)
                     'face '(:foreground "#B48EAD")))
       (if (= (user-uid) 0)
           (propertize " # " 'face '(:foreground "#BF616A" :weight bold))
         (propertize " ❯ " 'face '(:foreground "#A3BE8C" :weight bold))))))
  
  ;; Custom eshell functions
  
  ;; Fish-like "sudo !!" to repeat last command with sudo
  (defun eshell/!! ()
    "Repeat the last command with sudo."
    (let ((last-command (car (ring-elements eshell-history-ring))))
      (eshell-add-input (concat "sudo " last-command))))
  
  ;; Clear the eshell buffer
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  
  ;; Fish-like "ls" colorization with icons
  (defun eshell/ls (&rest args)
    "Replacement for `ls' with colorized output."
    (if (or (not args) (and (= (length args) 1) (string= (car args) "-l")))
        (let ((files (sort (directory-files "." nil 
                                            (lambda (file) 
                                              (not (or (string= file ".") (string= file ".."))))) 'string<)))
          (if files
              (let ((out ""))
                (dolist (file files)
                  (setq out (concat out 
                                   (cond
                                    ((file-directory-p file)
                                     (propertize file 'face '(:foreground "#8FBCBB" :weight bold)))
                                    ((file-executable-p file)
                                     (propertize file 'face '(:foreground "#A3BE8C")))
                                    (t (propertize file 'face '(:foreground "gray80"))))
                                   "\n")))
                (eshell-printn out))
            (eshell-printn "No files")))
      ;; Fall back to regular ls for other args
      (eshell-command-result (concat "ls " (string-join args " ")))))
  
  ;; Set up aliases (matching fish shell)
  (defun eshell-add-aliases ()
    (dolist (alias '(;; Editor
                     ("e" "$EDITOR $1")
                     ("vim" "find-file $1")
                     ("emacs" "find-file $1")
                     ("ff" "find-file $1")
                     ;; File operations with safety
                     ("cp" "cp -iv $*")
                     ("mv" "mv -iv $*")
                     ("rm" "rm -v $*")
                     ("mkdir" "mkdir -pv $*")
                     ("md" "mkdir -pv $1")
                     ;; Modern replacements
                     ("cat" "bat --style=plain $*")
                     ("diff" "diff --color=auto $*")
                     ;; Directory listing (eza)
                     ("la" "eza -a --icons --group-directories-first $*")
                     ("ll" "eza -la --icons --group-directories-first $*")
                     ("l" "eza --icons --group-directories-first $*")
                     ("lg" "eza -la --icons --group-directories-first --git $*")
                     ("lt" "eza -la --icons --group-directories-first --tree --level=2 $*")
                     ;; Navigation
                     (".." "cd ..")
                     ("..." "cd ../..")
                     (".3" "cd ../../..")
                     (".4" "cd ../../../..")
                     (".5" "cd ../../../../..")
                     ;; Utilities
                     ("c" "clear")
                     ("getip" "curl -s https://api.ipify.org")
                     ;; Kubernetes
                     ("kwatch" "watch kubectl $*")
                     ("kubectx" "kubectl config use-context $1")
                     ("kubens" "kubectl config set-context --current --namespace=$1")
                     ;; Git (use magit when possible)
                     ("gs" "magit-status")
                     ("gd" "magit-diff")
                     ("gb" "magit-branch")
                     ("gl" "magit-log")
                     ("gc" "magit-commit")
                     ;; File handling
                     ("open" "find-file $1")
                     ("d" "dired $1")
                     ("less" "view-file $1")
                     ;; Zoxide
                     ("cd" "z $1")
                     ("za" "zoxide add $1")
                     ("zr" "zoxide remove $1")
                     ("zq" "zoxide query $*")
                     ("zi" "zi")))
      (add-to-list 'eshell-command-aliases-list alias)))

  (add-hook 'eshell-mode-hook #'eshell-add-aliases)

  ;; ============================================================
  ;; Fish-like abbreviations (expand on space)
  ;; ============================================================
  (defvar eshell-abbrev-table
    '(;; System
      ("s" . "sudo ")
      ("se" . "sudo -e ")
      ("sy" . "systemctl ")
      ;; Git
      ("g" . "git ")
      ("ga" . "git add ")
      ("gdf" . "git diff ")
      ("gdc" . "git diff --cached ")
      ("gdx" . "git diff HEAD ")
      ("gf" . "git fetch ")
      ("gm" . "git merge ")
      ("gp" . "git pull ")
      ("gpx" . "git push ")
      ("gr" . "git rebase ")
      ("grx" . "git reset ")
      ("grb" . "git rebase ")
      ;; Kubernetes
      ("k" . "kubectl ")
      ("kgd" . "kubectl get deployments ")
      ("kg" . "kubectl get ")
      ("kgp" . "kubectl get pods ")
      ("kgh" . "kubectl get pods | head ")
      ("kge" . "kubectl get events --sort-by='.lastTimestamp' ")
      ("kgsv" . "kubectl get services ")
      ("ka" . "kubectl apply -f ")
      ("kde" . "kubectl describe ")
      ("kd" . "kubectl delete ")
      ("kgs" . "kubectl get secrets ")
      ("kl" . "kubectl logs ")
      ("kw" . "kubectl get pods --watch "))
    "Abbreviations for eshell, similar to fish abbreviations.")

  (defun eshell-expand-abbrev ()
    "Expand abbreviation at point if it matches, otherwise insert space."
    (interactive)
    (let* ((word-start (save-excursion (skip-chars-backward "a-zA-Z0-9") (point)))
           (word (buffer-substring-no-properties word-start (point)))
           (expansion (cdr (assoc word eshell-abbrev-table))))
      (if expansion
          (progn
            (delete-region word-start (point))
            (insert expansion))
        (insert " "))))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "SPC") 'eshell-expand-abbrev)))
  
  ;; Fish-like tab completion and extra keybindings
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq pcomplete-cycle-completions nil)
              (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
              (define-key eshell-mode-map (kbd "M-p") 'eshell-previous-matching-input-from-input)
              (define-key eshell-mode-map (kbd "M-n") 'eshell-next-matching-input-from-input)
              ;; Add keybindings for zoxide integration
              (define-key eshell-mode-map (kbd "C-c z") 'eshell/zf)
              (define-key eshell-mode-map (kbd "C-c C-z") 'eshell/zi)))
  
  ;; Syntax highlighting in eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (require 'esh-mode)
              (require 'eshell)
              (setenv "TERM" "xterm-256color")
              ;; Make command output read-only
              (add-hook 'eshell-output-filter-functions 'eshell-readonly-command-output-filter nil t))))

;; Eshell syntax highlighting
(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; Fish-like autosuggestions from history
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5))

;; ============================================================
;; Atuin integration for better history search
;; ============================================================

(defvar eshell-atuin-start-time nil
  "Start time of the current eshell command for duration tracking.")

(defun eshell-atuin-pre-command ()
  "Record start time before command execution."
  (setq eshell-atuin-start-time (current-time)))

(defun eshell-atuin-post-command (input)
  "Record command to atuin after execution.
INPUT is the command that was run."
  (when (and input
             (not (string-empty-p (string-trim input)))
             eshell-atuin-start-time)
    (let* ((duration (float-time (time-subtract (current-time) eshell-atuin-start-time)))
           (duration-ns (round (* duration 1000000000)))
           (exit-code (if (boundp 'eshell-last-command-status) eshell-last-command-status 0))
           (cwd (eshell/pwd)))
      ;; Record to atuin asynchronously
      (start-process "atuin-record" nil "atuin" "history" "start"
                     "--" input)
      ;; Note: Full atuin integration would need `atuin history end` with duration/exit code
      ;; but start is enough for basic history recording
      ))
  (setq eshell-atuin-start-time nil))

(defun eshell-atuin-history ()
  "Search eshell history using atuin interactively."
  (interactive)
  (let* ((atuin-output (shell-command-to-string "atuin search --cmd-only -i 2>/dev/null"))
         (selected (string-trim atuin-output)))
    (when (and selected (not (string-empty-p selected)))
      (eshell-bol)
      (delete-region (point) (line-end-position))
      (insert selected))))

(defun eshell-atuin-history-search ()
  "Search history with completing-read using atuin data."
  (interactive)
  (let* ((history-list (split-string
                        (shell-command-to-string "atuin history list --cmd-only --limit 500 2>/dev/null")
                        "\n" t))
         (selected (completing-read "Atuin history: " (delete-dups history-list) nil nil)))
    (when selected
      (eshell-bol)
      (delete-region (point) (line-end-position))
      (insert selected))))

(defun eshell-atuin-search-prefix ()
  "Search atuin history filtered by current input."
  (interactive)
  (let* ((current-input (buffer-substring-no-properties
                         (eshell-bol) (line-end-position)))
         (search-cmd (if (string-empty-p current-input)
                         "atuin history list --cmd-only --limit 500 2>/dev/null"
                       (format "atuin search --cmd-only --search-mode prefix '%s' 2>/dev/null"
                               (shell-quote-argument current-input))))
         (history-list (split-string (shell-command-to-string search-cmd) "\n" t))
         (selected (completing-read "History: " (delete-dups history-list) nil nil current-input)))
    (when selected
      (eshell-bol)
      (delete-region (point) (line-end-position))
      (insert selected))))

;; Hook up atuin recording
(add-hook 'eshell-pre-command-hook #'eshell-atuin-pre-command)
(add-hook 'eshell-post-command-hook
          (lambda () (eshell-atuin-post-command (ring-ref eshell-history-ring 0))))

;; Keybindings for atuin
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-r") 'eshell-atuin-search-prefix)
            (define-key eshell-mode-map (kbd "M-r") 'eshell-atuin-history-search)
            (define-key eshell-mode-map (kbd "C-S-r") 'eshell-atuin-history)))

;; Integrate with system zoxide for smart directory jumping
(defun eshell/z (&rest args)
  "Use zoxide to jump to a directory matching patterns in ARGS."
  (let ((zoxide-cmd (concat "zoxide query " (string-join args " "))))
    (let ((target-dir (string-trim (shell-command-to-string zoxide-cmd))))
      (if (file-directory-p target-dir)
          (eshell/cd target-dir)
        (eshell-print (format "No match found for %s\n" (string-join args " ")))))))

(defun eshell/zi (&rest args)
  "Use zoxide interactive mode to jump to a directory."
  (let ((result (shell-command-to-string "zoxide query -i")))
    (let ((target-dir (string-trim result)))
      (when (file-directory-p target-dir)
        (eshell/cd target-dir)))))

;; Add hooks to notify zoxide of directory changes
(defun update-zoxide-database ()
  "Add current directory to zoxide database."
  (when (and (not (string= (eshell/pwd) "~")) 
             (file-directory-p (eshell/pwd)))
    (start-process "zoxide-add" nil "zoxide" "add" (eshell/pwd))))

(add-hook 'eshell-directory-change-hook 'update-zoxide-database)

;; Enhanced eshell/cd to integrate with zoxide
(advice-add 'eshell/cd :around
            (lambda (orig-fun &rest args)
              "Use zoxide when no arguments, otherwise use standard cd behavior."
              (if (not args)
                  ;; When no args, just go home like normal cd
                  (apply orig-fun '("~"))
                ;; Try zoxide first, fall back to regular cd
                (condition-case nil
                    (eshell/z (car args))
                  (error (apply orig-fun args))))))

;; Add fzf-like zoxide selection with completion
(defun eshell/zf (&rest _args)
  "Use zoxide with fzf-like selection using Emacs completion."
  (interactive)
  (let* ((dirs (split-string (shell-command-to-string "zoxide query --list") "\n" t))
         (selected (completing-read "Jump to: " dirs nil t)))
    (when selected
      (eshell/cd selected))))

;; Fish-like prompt - disabled to avoid dependency issues
;; (use-package eshell-prompt-extras
;;   :after eshell
;;   :config
;;   (with-eval-after-load "em-prompt"
;;     (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;     (setq eshell-highlight-prompt nil
;;           eshell-prompt-function 'epe-theme-lambda)))

;; Fish-like directory jumping (up to parent by name)
(use-package eshell-up
  :commands (eshell-up eshell-up-peek)
  :config
  (defalias 'eshell/up #'eshell-up)
  (defalias 'eshell/pk #'eshell-up-peek))

;; Useful functions for working with Eshell

;; Open Eshell in current directory
(defun eshell-here ()
  "Opens a new eshell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                    (file-name-directory (buffer-file-name))
                  default-directory))
         (name (car (last (split-string parent "/" t)))))
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

;; Open Eshell in a split
(defun split-and-eshell-here ()
  "Split window and open eshell in current directory."
  (interactive)
  (split-window-below)
  (other-window 1)
  (eshell-here))

;; Open Eshell in a side window
(defun eshell-toggle ()
  "Toggle eshell in a side window."
  (interactive)
  (if (get-buffer-window "*eshell-toggle*")
      (delete-window (get-buffer-window "*eshell-toggle*"))
    (let ((buf (get-buffer "*eshell-toggle*")))
      (unless buf
        (setq buf (generate-new-buffer "*eshell-toggle*"))
        (with-current-buffer buf
          (eshell-mode)))
      (display-buffer buf
                      '((display-buffer-in-side-window)
                        (side . bottom)
                        (window-height . 0.3))))))

;; Function to spawn a new eshell in project root
(defun eshell-project-root ()
  "Open eshell in the project root directory."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (eshell "new")
    (rename-buffer (format "*eshell: %s*" (file-name-nondirectory (directory-file-name default-directory))))))

;; Global key bindings for Eshell
(global-set-key (kbd "C-c e e") 'eshell)
(global-set-key (kbd "C-c e h") 'eshell-here)
(global-set-key (kbd "C-c e s") 'split-and-eshell-here)
(global-set-key (kbd "C-c e t") 'eshell-toggle)
(global-set-key (kbd "C-c e p") 'eshell-project-root)

;; Global zoxide command for finding directories anywhere in Emacs
(defun find-file-zoxide ()
  "Use zoxide to find a directory, then find a file there using completion."
  (interactive)
  (let* ((dirs (split-string (shell-command-to-string "zoxide query --list") "\n" t))
         (selected-dir (completing-read "Directory: " dirs nil t)))
    (when selected-dir
      ;; Add to zoxide database
      (start-process "zoxide-add" nil "zoxide" "add" selected-dir)
      ;; Use find-file with the selected directory as default-directory
      (let ((default-directory selected-dir))
        (call-interactively 'find-file)))))

;; Global keybinding for zoxide directory jump
(global-set-key (kbd "C-c j") 'find-file-zoxide)

;; General.el integration for eshell commands
(with-eval-after-load 'general
  (when (fboundp 'my/leader-keys)
    (my/leader-keys
      ;; Eshell operations
      "e"   '(:ignore t :which-key "eshell")
      "ee"  '(eshell :which-key "eshell")
      "eh"  '(eshell-here :which-key "eshell here")
      "es"  '(split-and-eshell-here :which-key "eshell split")
      "et"  '(eshell-toggle :which-key "eshell toggle")
      "ep"  '(eshell-project-root :which-key "eshell project root")
      
      ;; Zoxide commands
      "ez"  '(:ignore t :which-key "zoxide")
      "ezf" '(find-file-zoxide :which-key "zoxide find file")
      "ezi" '(eshell/zi :which-key "zoxide interactive"))))

;; Update which-key descriptions
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC e" "eshell")
  (which-key-add-key-based-replacements "SPC ez" "zoxide"))

;; ============================================================
;; Direnv integration
;; ============================================================
(use-package envrc
  :hook (eshell-mode . envrc-mode)
  :config
  (envrc-global-mode))

;; ============================================================
;; Additional fish-like functions
;; ============================================================

;; Fish-like `mcd` - make directory and cd into it
(defun eshell/mcd (dir)
  "Create DIR and change to it."
  (make-directory dir t)
  (eshell/cd dir))

;; Fish-like `take` - alias for mcd
(defalias 'eshell/take #'eshell/mcd)

;; Better `which` that shows the full path
(defun eshell/which (cmd)
  "Show full path of CMD."
  (eshell-printn (executable-find cmd)))

;; Quick file preview
(defun eshell/preview (file)
  "Preview FILE in a popup buffer."
  (view-file file))

(provide 'init.eshell)