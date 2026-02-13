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
  (setq eshell-prompt-regexp "^[^❯#\n]*[❯#] "
        eshell-highlight-prompt nil)

  ;; Starship-like prompt with git info, duration, and status
  (defvar my/eshell-last-cmd-start nil)
  (defvar my/eshell-last-cmd-duration nil)

  (defun my/eshell--git-info ()
    "Return git branch + status string or nil."
    (let ((branch (string-trim
                   (shell-command-to-string
                    "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))
      (when (and (not (string-empty-p branch))
                 (not (string-prefix-p "fatal" branch)))
        (let ((dirty (not (string-empty-p
                           (string-trim
                            (shell-command-to-string
                             "git status --porcelain 2>/dev/null"))))))
          (concat
           (propertize " " 'face '(:foreground "#444444"))
           (propertize " " 'face '(:foreground "#B48EAD"))
           (propertize branch 'face '(:foreground "#B48EAD"))
           (when dirty
             (propertize " ✗" 'face '(:foreground "#EBCB8B"))))))))

  (defun my/eshell--node-version ()
    "Return node version if package.json exists."
    (when (locate-dominating-file "." "package.json")
      (let ((ver (string-trim (shell-command-to-string "node --version 2>/dev/null"))))
        (unless (string-empty-p ver)
          (concat " " (propertize (concat "⬢ " ver) 'face '(:foreground "#5e81ac")))))))

  (defun my/eshell--python-version ()
    "Return python version if in a python project."
    (when (or (locate-dominating-file "." "pyproject.toml")
              (locate-dominating-file "." "setup.py")
              (locate-dominating-file "." ".python-version"))
      (let ((ver (string-trim (shell-command-to-string "python --version 2>/dev/null"))))
        (unless (string-empty-p ver)
          (concat " " (propertize (concat "🐍 " (string-trim-left ver "Python "))
                                  'face '(:foreground "#EBCB8B")))))))

  (defun my/eshell--kube-context ()
    "Return current kube context if KUBECONFIG is set."
    (when (getenv "KUBECONFIG")
      (let ((ctx (string-trim (shell-command-to-string "kubectl config current-context 2>/dev/null"))))
        (unless (string-empty-p ctx)
          (concat " " (propertize (concat "☸ " ctx) 'face '(:foreground "#88C0D0")))))))

  (defun my/eshell--duration ()
    "Return duration string if last command took >2s."
    (when (and my/eshell-last-cmd-duration (> my/eshell-last-cmd-duration 2))
      (concat " "
              (propertize (format "took %.1fs" my/eshell-last-cmd-duration)
                          'face '(:foreground "#4C566A")))))

  (defun my/eshell-prompt ()
    "Starship-like multi-line prompt."
    (let* ((dir (propertize (abbreviate-file-name (eshell/pwd))
                            'face '(:foreground "#8FBCBB" :weight bold)))
           (git (my/eshell--git-info))
           (node (ignore-errors (my/eshell--node-version)))
           (py (ignore-errors (my/eshell--python-version)))
           (kube (ignore-errors (my/eshell--kube-context)))
           (dur (my/eshell--duration))
           (status (if (and (boundp 'eshell-last-command-status)
                            (not (zerop eshell-last-command-status)))
                       (propertize "❯ " 'face '(:foreground "#BF616A" :weight bold))
                     (propertize "❯ " 'face '(:foreground "#A3BE8C" :weight bold)))))
      (setq my/eshell-last-cmd-duration nil)
      (concat "\n" dir (or git "") (or node "") (or py "") (or kube "") (or dur "")
              "\n"
              (if (= (user-uid) 0)
                  (propertize "# " 'face '(:foreground "#BF616A" :weight bold))
                status))))

  (setq eshell-prompt-function #'my/eshell-prompt)

  (add-hook 'eshell-pre-command-hook
            (lambda () (setq my/eshell-last-cmd-start (current-time))))
  (add-hook 'eshell-post-command-hook
            (lambda ()
              (when my/eshell-last-cmd-start
                (setq my/eshell-last-cmd-duration
                      (float-time (time-subtract (current-time) my/eshell-last-cmd-start))))))

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
  
  ;; Set up aliases (matching fish shell)
  (defun eshell-add-aliases ()
    (dolist (alias '(;; Editor
                     ("e" "$EDITOR $1")
                     ("vim" "find-file $1")
                     ("emacs" "find-file $1")
                     ("ff" "find-file $1")
                     ;; File operations (match zsh)
                     ("cp" "cp -airv $*")
                     ("mv" "mv -iv $*")
                     ("rm" "rm -rf $*")
                     ("ln" "ln -vi $*")
                     ("mkdir" "mkdir -p $*")
                     ("md" "mkdir -p $1")
                     ;; Modern replacements
                     ("cat" "bat $*")
                     ("diff" "diff --color=auto $*")
                     ("rg" "rg --color always $*")
                     ("docker" "podman $*")
                     ;; Directory listing (eza, match zsh)
                     ("la" "eza -a --group-directories-first $*")
                     ("ll" "eza -la --group-directories-first --icons $*")
                     ("l" "eza -la --group-directories-first --icons $*")
                     ("lg" "eza -l --group-directories-first --ignore-glob='.git' --icons $*")
                     ("lt" "eza -T --group-directories-first --level=4 $*")
                     ("l." "eza -a $* | grep -E '^\\.'" )
                     ;; Navigation
                     (".." "cd ..")
                     ("..." "cd ../..")
                     (".3" "cd ../../..")
                     (".4" "cd ../../../..")
                     (".5" "cd ../../../../..")
                     ("back" "cd $OLDPWD")
                     ;; Utilities
                     ("c" "clear")
                     ("getip" "curl -s https://ifconfig.me")
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
      ("sy" . "sudo systemctl ")
      ;; Git
      ("g" . "git ")
      ("ga" . "git add ")
      ("gc" . "git commit -m ")
      ("gdc" . "git diff --cached ")
      ("gdf" . "git diff --name-only ")
      ("gdx" . "git rm -r ")
      ("gf" . "git fetch ")
      ("gl" . "git log ")
      ("gm" . "git merge ")
      ("gox" . "git open ")
      ("gp" . "git pull ")
      ("gpx" . "git push ")
      ("gr" . "git restore ")
      ("grb" . "git rebase ")
      ("grx" . "git rm -r ")
      ("gs" . "git status ")
      ;; Kubernetes
      ("k" . "kubectl ")
      ("ka" . "kubectl apply -f ")
      ("kd" . "kubectl describe ")
      ("kde" . "kubectl delete ")
      ("kex" . "kubectl exec -it ")
      ("kg" . "kubectl get ")
      ("kgd" . "kubectl get deployments -o wide ")
      ("kge" . "kubectl get events --watch ")
      ("kgh" . "kubectl get hr -o wide ")
      ("kgp" . "kubectl get pod ")
      ("kgs" . "kubectl get service ")
      ("kgsv" . "kubectl get service -o wide ")
      ("kgw" . "kubectl get pod --watch ")
      ("kl" . "kubectl logs -f ")
      ("kw" . "watch kubectl get -f ")
      ;; Portage
      ("eu" . "sudo emerge --update --deep --newuse @world ")
      ("ei" . "sudo emerge ")
      ("es" . "eix "))
    "Abbreviations for eshell, synced with zsh abbreviations.")

  (defun eshell-expand-abbrev ()
    "Expand abbreviation at point if it matches, otherwise insert space."
    (interactive)
    (let* ((word-start (save-excursion (eshell-bol) (point)))
           (word (string-trim (buffer-substring-no-properties word-start (point))))
           (expansion (cdr (assoc word eshell-abbrev-table))))
      (if expansion
          (progn
            (delete-region word-start (point))
            (insert expansion))
        (insert " "))))

  ;; Also register abbreviations as eshell aliases so they work on Enter
  (defun my/eshell-add-abbrev-aliases ()
    "Generate eshell aliases from abbreviation table."
    (dolist (abbrev eshell-abbrev-table)
      (let* ((name (car abbrev))
             (expansion (string-trim (cdr abbrev)))
             (parts (split-string expansion))
             (cmd (car parts))
             (args (cdr parts)))
        (add-to-list 'eshell-command-aliases-list
                     (list name (concat (string-join (cons cmd args) " ") " $*"))))))

  (add-hook 'eshell-mode-hook #'my/eshell-add-abbrev-aliases)

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
  
  ;; Ensure TERM is set for color output
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setenv "TERM" "xterm-256color"))))

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
          (lambda ()
            (when (and (bound-and-true-p eshell-history-ring)
                       (not (ring-empty-p eshell-history-ring)))
              (eshell-atuin-post-command (ring-ref eshell-history-ring 0)))))

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

;; Update which-key descriptions
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC e" "eshell")
  (which-key-add-key-based-replacements "SPC ez" "zoxide"))

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