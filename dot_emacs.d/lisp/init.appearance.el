;; -*- lexical-binding: t; -*-
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
           :default-family "BerkeleyMono Nerd Font"
           :default-height 105
           :variable-pitch-family "BerkeleyMono Nerd Font"
           :variable-pitch-height 115
           :fixed-pitch-family "BerkeleyMono Nerd Font"
           :fixed-pitch-height 105
           :line-spacing 1))))

(unless (daemonp)
  (when (display-graphic-p)
    (fontaine-set-preset 'regular)))

(setq-default line-spacing 1)
;; Doom IR Black - pure black background (bg #000000), based on the classic
;; ir_black theme, with a clearly distinct mode-line (not blended into bg
;; like doom-homage-black's minimal "homage" design). doom-ir-black-brighter-
;; comments is the theme's own supported customization (not a hand-picked
;; hex patch) taking comment contrast from 3.4:1 to 6.6:1.
(use-package doom-themes
  :demand t
  :init
  (setq doom-ir-black-brighter-comments t)
  :config
  (load-theme 'doom-ir-black t))

;; doom-ir-black's own hl-line (bg-alt, #121212) is only ~1.1:1 contrast
;; against pure black - barely visible. This is the one deliberate manual
;; override: keep it monochrome (no colored tint, unlike the old pitch-black
;; overlay's navy-ish choices), just bright enough to actually see. GUI only:
;; terminal frames already disable global-hl-line-mode in my/terminal-setup.
;; with-eval-after-load, not a plain set-face-attribute here: the hl-line
;; face doesn't exist until hl-line.el actually loads (later in this file),
;; so calling this directly at top level fails with "Invalid face: hl-line".
(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil :background "#1a1a1a" :extend t))


;; Man-mode colors
(use-package man
  :ensure nil
  :defer t
  :custom
  (Man-notify-method 'pushy)
  :custom-face
  (Man-overstrike ((t :foreground "#55B5DB" :weight bold)))
  (Man-underline ((t :foreground "#9FCA56" :underline t)))
  (Man-reverse ((t :foreground "#000000" :background "#D4D7D6"))))

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
;; Mode line: doom-modeline
;; ============================================================
;; Single maintained package instead of moody+cyphejor+minions+which-func+mlscroll.
;; Handles GUI/terminal/daemon frames itself — no per-frame face overrides needed.
;; which-func is intentionally not shown here: breadcrumb-mode's header-line
;; already gives an imenu-based "what scope am I in" trail.
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 24))

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
      (if-let* ((proj (and (fboundp 'projectile-project-root)
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
(use-package visual-fill-column)

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
  "Configure Emacs for terminal frames.
Keeps doom-homage-black foreground/syntax colors but lets the
terminal supply its own background (transparent)."
  (unless (display-graphic-p)
    (let ((frame (selected-frame)))
      ;; Only clear the default face background so the terminal's own
      ;; background (transparency) shows through. Leave all other faces
      ;; (including mode-line) using their theme colors — kitty supports
      ;; 24-bit color so they render correctly.
      ;; "unspecified-bg" is the Emacs sentinel for "use terminal's own background".
      ;; The symbol 'unspecified only means "inherit from parent face/theme" and
      ;; would fall back to whatever the active theme set (e.g. #000000 from pitch-black).
      (set-face-attribute 'default frame :background "unspecified-bg"))
    ;; Disable hl-line — it still tints even with unspecified-bg on some terminals
    (global-hl-line-mode -1)
    ;; Disable GUI-only modes
    (when (bound-and-true-p spacious-padding-mode)
      (spacious-padding-mode -1))
    (when (bound-and-true-p breadcrumb-mode)
      (breadcrumb-mode -1))
    (when (bound-and-true-p solaire-global-mode)
      (solaire-global-mode -1))
    (global-tab-line-mode -1)
    (xterm-mouse-mode 1)))

;; ============================================================
;; Frame setup: apply theme + display modes per frame type
;; ============================================================

(defun my/setup-frame (frame)
  "Apply GUI/terminal-specific tweaks per frame type.
doom-homage-black is already pure black on its own (no overlay theme
needed), so the graphical branch only needs to match the frame chrome
to that, not introduce a color of its own."
  (if (frame-parameter frame 'window-system)
      (progn
        (set-frame-parameter frame 'background-color "#000000")
        (force-mode-line-update t))
    ;; Terminal frame: transparent mode-line, matching the buffer background.
    ;; nil frame = all frames, above-theme priority but below graphical frame-local.
    (set-face-attribute 'mode-line nil :background "unspecified-bg" :box nil)
    (set-face-attribute 'mode-line-active nil :background "unspecified-bg" :box nil)
    (set-face-attribute 'mode-line-inactive nil :background "unspecified-bg" :box nil)
    (with-selected-frame frame
      (my/terminal-setup)
      (force-mode-line-update t))))

;; Fires for every new frame: daemon emacsclient connections and
;; additional frames in a running session.
(add-hook 'after-make-frame-functions #'my/setup-frame)

;; Fires after init for the initial frame in non-daemon graphical mode.
(unless (daemonp)
  (add-hook 'window-setup-hook
            (lambda () (my/setup-frame (selected-frame)))))

;; ============================================================
;; Daemon: one-time heavy setup on first graphical frame
;; ============================================================
(when (daemonp)
  (defvar my/daemon-display-initialized nil)
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (when (and (display-graphic-p)
                         (not my/daemon-display-initialized))
                (setq my/daemon-display-initialized t)
                (fontaine-set-preset 'regular)
                (spacious-padding-mode 1)
                (solaire-global-mode +1)
                (breadcrumb-mode 1)
                (global-tab-line-mode 1)
                (my/setup-frame (selected-frame))))))

(provide 'init.appearance)
