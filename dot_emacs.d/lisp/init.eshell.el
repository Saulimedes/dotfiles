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
          "dstat" "watch" "tail" "ssh" "zsh" "bash" "nmtui" "alsamixer" "ranger"))
  
  ;; Configure eshell prompt
  (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
  
  ;; Define a pretty prompt with git info (like Fish)
  (defun eshell-prompt-function ()
    (let ((current-branch (magit-get-current-branch)))
      (concat
       (propertize (format-time-string "[%H:%M:%S] " (current-time))
                   'face '(:foreground "gray60"))
       (propertize (abbreviate-file-name (eshell/pwd))
                   'face '(:foreground "#8FBCBB" :weight bold))
       (when current-branch
         (propertize (format " (%s)" current-branch)
                     'face '(:foreground "#B48EAD")))
       (if (= (user-uid) 0)
           (propertize " # " 'face '(:foreground "red" :weight bold))
         (propertize " $ " 'face '(:foreground "#A3BE8C" :weight bold))))))
  
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
  
  ;; Set up aliases
  (defun eshell-add-aliases ()
    (dolist (alias '(("ll" "ls -lah")
                     ("la" "ls -a")
                     ("vim" "find-file $1")
                     ("emacs" "find-file $1")
                     ("e" "find-file $1")
                     ("ff" "find-file $1")
                     ("d" "dired $1")
                     ("less" "view-file $1")
                     ("md" "mkdir -p $1")
                     ("c" "clear")
                     ;; Git aliases
                     ("gs" "magit-status")
                     ("gd" "magit-diff")
                     ("gb" "magit-branch")
                     ("gl" "magit-log")
                     ("gc" "magit-commit")
                     ;; File handling
                     ("cat" "view-file $1")
                     ("open" "find-file $1")
                     ("cdi" "dired-other-window $1")
                     ;; Zoxide equivalents (zsh/bash compatible)
                     ("cd" "z $1") ; Override cd to use zoxide when possible
                     ("za" "zoxide add $1")
                     ("zr" "zoxide remove $1")
                     ("zq" "zoxide query $*")
                     ("zi" "zi"))) ; Interactive zoxide
      (add-to-list 'eshell-command-aliases-list alias)))
  
  (add-hook 'eshell-mode-hook #'eshell-add-aliases)
  
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

;; Eshell syntax highlighting with shell-like features - disabled to avoid dependency issues
;; (use-package eshell-syntax-highlighting
;;   :after eshell
;;   :config
;;   (eshell-syntax-highlighting-global-mode +1))

;; Fish-like history searching - disabled to avoid dependency issues
;; (use-package esh-autosuggest
;;   :hook (eshell-mode . esh-autosuggest-mode))

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

;; Fish-like directory jumping - disabled to avoid dependency issues
;; (use-package eshell-up
;;   :config
;;   (defalias 'eshell/up #'eshell-up)
;;   (defalias 'eshell/pk #'eshell-up-peek))

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

(provide 'init.eshell)