;; Frame dimensions (push to preserve early-init.el settings)
(push '(width . 160) default-frame-alist)
(push '(height . 60) default-frame-alist)

;; Better frame behavior
(setq frame-title-format '("%b - Emacs")
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Font setup
(use-package fontaine
  :init
  (setq fontaine-presets
        '((regular
           :default-family "MonoLisaVariable Nerd Font"
           :default-height 105
           :variable-pitch-family "SF Pro"
           :variable-pitch-height 115
           :fixed-pitch-family "MonoLisaVariable Nerd Font"
           :fixed-pitch-height 105
           :line-spacing 1))))

(unless (daemonp)
  (fontaine-set-preset 'regular))

(setq-default line-spacing 1)
;; Tomorrow Night Theme - clean, colorful syntax highlighting
(use-package color-theme-sanityinc-tomorrow
  :demand t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

;; Define pitch-black as a proper overlay theme (cleanly toggleable)
(deftheme my-pitch-black "Pitch black background overlay.")
(custom-theme-set-faces
 'my-pitch-black
 ;; Core backgrounds
 '(default ((t :background "#000000")))
 '(fringe ((t :background "#000000")))
 '(cursor ((t :background "#ffffff")))
 '(hl-line ((t :background "#0e0e0e")))
 '(region ((t :background "#3b4252")))
 '(vertical-border ((t :foreground "#1a1a1a")))
 ;; Line numbers
 '(line-number ((t :background "#000000")))
 '(line-number-current-line ((t :background "#0e0e0e")))
 ;; Header / tab line
 '(header-line ((t :background "#000000")))
 '(tab-line ((t :background "#000000")))
 ;; Minibuffer
 '(minibuffer-prompt ((t :background "#000000")))
 ;; Modeline - visible against black
 '(mode-line ((t :background "#1c1c1c" :foreground "#c5c8c6" :box (:line-width (1 . 4) :color "#1c1c1c") :overline nil :underline nil)))
 '(mode-line-inactive ((t :background "#111111" :foreground "#555555" :box (:line-width (1 . 4) :color "#111111") :overline nil :underline nil)))
 '(mode-line-highlight ((t :background "#ffffff" :foreground "#000000" :box nil)))
 ;; Doom-modeline segments
 '(doom-modeline-bar ((t :background "#81a2be")))
 '(doom-modeline-bar-inactive ((t :background "#373b41")))
 '(doom-modeline-panel ((t :background "#1c1c1c")))
 '(doom-modeline-buffer-file ((t :background unspecified)))
 '(doom-modeline-buffer-modified ((t :background unspecified)))
 '(doom-modeline-buffer-path ((t :background unspecified)))
 '(doom-modeline-project-dir ((t :background unspecified)))
 '(doom-modeline-highlight ((t :background "#ffffff" :foreground "#000000")))
 '(doom-modeline-warning ((t :background unspecified :inherit doom-modeline)))
 '(doom-modeline-urgent ((t :background unspecified :inherit doom-modeline)))
 '(doom-modeline-info ((t :background unspecified :inherit doom-modeline)))
 '(doom-modeline-notification ((t :background unspecified)))
 '(doom-modeline-buffer-major-mode ((t :background unspecified)))
 '(warning ((t :background unspecified)))
 ;; Solaire (non-file buffers)
 '(solaire-default-face ((t :background "#000000")))
 '(solaire-fringe-face ((t :background "#000000")))
 '(solaire-header-line-face ((t :background "#000000")))
 '(solaire-mode-line-face ((t :background "#1c1c1c")))
 '(solaire-mode-line-inactive-face ((t :background "#111111"))))
(provide-theme 'my-pitch-black)
(enable-theme 'my-pitch-black)

(global-set-key (kbd "<f5>") (lambda () (interactive)
                               (let ((themes '(sanityinc-tomorrow-night
                                               sanityinc-tomorrow-day
                                               sanityinc-tomorrow-eighties
                                               sanityinc-tomorrow-blue
                                               sanityinc-tomorrow-bright))
                                     (current (car custom-enabled-themes)))
                                 (mapc #'disable-theme custom-enabled-themes)
                                 (let ((next (or (cadr (member current themes)) (car themes))))
                                   (load-theme next t)
                                   (when (eq next 'sanityinc-tomorrow-night)
                                     (enable-theme 'my-pitch-black))
                                   (message "Theme: %s" next)))))

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

;; pulsar - pulse line on jump (highlights whole line)
(use-package pulsar
  :init
  (setq pulsar-pulse t
        pulsar-delay 0.03
        pulsar-iterations 5
        pulsar-face 'pulsar-cyan)
  :config
  (pulsar-global-mode 1))

;; beacon - cursor trail effect (like shell cursor trail)
(use-package beacon
  :diminish beacon-mode
  :init
  (setq beacon-size 40              ; Size of the beacon
        beacon-color "#5e81ac"      ; Nord-ish blue, change to your theme
        beacon-blink-duration 0.3   ; How long the trail lasts
        beacon-blink-delay 0.1      ; Delay before blinking
        beacon-blink-when-window-scrolls t
        beacon-blink-when-window-changes t
        beacon-blink-when-point-moves-vertically 3) ; Only on big jumps (3+ lines)
  :config
  (beacon-mode 1)
  ;; Don't beacon in these modes
  (add-to-list 'beacon-dont-blink-major-modes 'vterm-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'term-mode))

;; line numbers with better performance
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative
        display-line-numbers-width-start t))

;; Highlight indentation levels
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0.1))

;; ============================================================
;; Doom Modeline - modern config
;; ============================================================
(use-package doom-modeline
  :init
  (setq doom-modeline-height 30
        doom-modeline-bar-width 4
        doom-modeline-hud nil
        doom-modeline-window-width-limit 85
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-lsp-icon t
        doom-modeline-time t
        doom-modeline-time-icon nil
        doom-modeline-battery nil
        doom-modeline-env-version t
        doom-modeline-vcs-max-length 24
        doom-modeline-persp-name nil
        doom-modeline-modal t
        doom-modeline-modal-icon t
        doom-modeline-modal-modern-icon t)
  :config
  (setq display-time-format "%H:%M"
        display-time-default-load-average nil)
  (display-time-mode 1)
  (unless (daemonp)
    (doom-modeline-mode 1)))

;; Clean modeline padding
(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 12
          :header-line-width 4
          :mode-line-width 0
          :tab-width 4
          :right-divider-width 16
          :scroll-bar-width 0
          :fringe-width 8))
  (unless (daemonp)
    (spacious-padding-mode 1)))

;; ============================================================
;; Solaire-mode - dim non-file buffers (VSCode-like)
;; ============================================================
(use-package solaire-mode
  :config
  (unless (daemonp)
    (solaire-global-mode +1)))

;; ============================================================
;; Git blame on-demand
;; ============================================================
(defun my/show-git-blame ()
  "Show git blame info for current line in minibuffer."
  (interactive)
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
                (summary (match-string 6 git-output)))
            (unless (string= commit-hash "0000000000000000000000000000000000000000")
              (message "Blame: %s - %s (%s)" author summary (substring commit-hash 0 8))))
        (message "No git blame info for this line")))))

(global-set-key (kbd "C-c g b") 'my/show-git-blame)

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


;; page-break-lines-mode
(use-package page-break-lines
  :if (display-graphic-p))

;; ============================================================
;; Tab Line - built-in buffer tabs
;; ============================================================
(use-package tab-line
  :ensure nil
  :init
  (setq tab-line-close-button-show t
        tab-line-new-button-show nil
        tab-line-separator " "
        tab-line-tab-name-truncated-max 20)
  :config
  (unless (daemonp)
    (if (display-graphic-p)
        (global-tab-line-mode 1)
      (global-tab-line-mode -1)))
  ;; Filter out special buffers
  (setq tab-line-exclude-modes
        '(special-mode completion-list-mode help-mode
          messages-buffer-mode magit-mode))
  (defun my/tab-line-buffer-group (buffer)
    "Group tabs by project."
    (with-current-buffer buffer
      (if-let ((proj (and (fboundp 'projectile-project-root)
                          (projectile-project-root))))
          proj
        "general")))
  (setq tab-line-tabs-buffer-group-function #'my/tab-line-buffer-group)
  (setq tab-line-tabs-function #'tab-line-tabs-buffer-groups)
)

;; ============================================================
;; Breadcrumb - VSCode-like file path in header
;; ============================================================
(use-package breadcrumb
  :config
  (unless (daemonp)
    (breadcrumb-mode 1)))

;; ============================================================
;; Symbol overlay - highlight symbol at point (like VSCode)
;; ============================================================
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-n" . symbol-overlay-switch-forward)
              ("M-p" . symbol-overlay-switch-backward)
              ("M-c" . symbol-overlay-remove-all)))

;; ============================================================
;; Rainbow mode - colorize color strings
;; ============================================================
(use-package rainbow-mode
  :diminish
  :hook ((css-mode scss-mode html-mode web-mode) . rainbow-mode))

;; ============================================================
;; Visual fill column - soft wrap at fill-column (nice for prose)
;; ============================================================
(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-width 100))

;; ============================================================
;; Save cursor position in files
;; ============================================================
(use-package saveplace
  :ensure nil
  :straight nil
  :init
  (setq save-place-file (expand-file-name "places" emacs-cache-directory))
  :config
  (save-place-mode 1))

;; ============================================================
;; Terminal: use terminal's own background colors
;; ============================================================
(defun my/terminal-setup ()
  "Configure Emacs for terminal frames."
  (unless (display-graphic-p)
    ;; Disable pitch-black overlay so all its #000000 backgrounds go away
    (disable-theme 'my-pitch-black)
    ;; Use terminal's own background
    (set-face-background 'default "unspecified-bg")
    (set-face-background 'line-number "unspecified-bg")
    (set-face-background 'line-number-current-line "unspecified-bg")
    (set-face-background 'fringe "unspecified-bg")
    (set-face-background 'hl-line "unspecified-bg")
    (set-face-background 'mode-line "unspecified-bg")
    (set-face-background 'mode-line-inactive "unspecified-bg")
    (set-face-background 'header-line "unspecified-bg")
    (set-face-background 'minibuffer-prompt "unspecified-bg")
    ;; Disable tab line (clicks cause buffer switching)
    (global-tab-line-mode -1)
    (xterm-mouse-mode 1)))

(add-hook 'tty-setup-hook #'my/terminal-setup)
;; Also run for daemon frames created via emacsclient -t
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (my/terminal-setup))))

;; ============================================================
;; Daemon: defer display modes until first frame is created
;; ============================================================
(when (daemonp)
  (defvar my/daemon-display-initialized nil)
  (defun my/daemon-setup-display (frame)
    "Activate display-dependent modes on first frame creation."
    (unless my/daemon-display-initialized
      (setq my/daemon-display-initialized t)
      (with-selected-frame frame
        (fontaine-set-preset 'regular)
        (doom-modeline-mode 1)
        (spacious-padding-mode 1)
        (solaire-global-mode +1)
        (breadcrumb-mode 1)
        (when (display-graphic-p)
          (global-tab-line-mode 1)))))
  (add-hook 'server-after-make-frame-hook
            (lambda () (my/daemon-setup-display (selected-frame)))))

(provide 'init.appearance)
