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
           :default-weight regular
           :default-height 120
           :variable-pitch-family "BerkeleyMono Nerd Font"
           :variable-pitch-height 125
           :fixed-pitch-family "BerkeleyMono Nerd Font"
           :fixed-pitch-weight regular
           :fixed-pitch-height 120
           :line-spacing 0.2))))

(unless (daemonp)
  (when (display-graphic-p)
    (fontaine-set-preset 'regular)))

(setq-default line-spacing 0.2)

;; Switching between a light and a dark theme (`modus-themes-toggle') changes
;; `frame-background-mode', and Emacs then re-checks every frame's faces. The
;; daemon's invisible initial frame never realizes faces defined after it
;; was created, so that check dies with "Invalid face: appt-notification",
;; aborts the switch and leaves the old theme enabled.
(define-advice frame-set-background-mode (:around (orig frame &rest args) my/skip-daemon-initial-frame)
  (unless (equal (terminal-name (frame-terminal frame)) "initial_terminal")
    (apply orig frame args)))

;; modus-operandi (modus-themes), not spacemacs-light: spacemacs-light
;; leans lavender/purple. modus-themes' palette is neutral and
;; high-contrast with no purple tint, and it's a well-maintained,
;; accessibility-focused theme bundled with Emacs itself. SPC t t toggles
;; to its dark twin modus-vivendi (`modus-themes-toggle'); terminal frames
;; always use the dark one (see "Terminal" below).
(use-package modus-themes
  :demand t
  :init
  ;; Flat modeline (the default mid-grey bar is heavy against the
  ;; flat bar-and-text segments in the modeline section below). Border =
  ;; bar color on purpose: Emacs outlines a hovered run in the face's :box,
  ;; so any visible border color shows up as a box around the hovered segment.
  (setq modus-themes-common-palette-overrides
        '((bg-mode-line-active bg-dim)
          (border-mode-line-active bg-dim)
          (bg-mode-line-inactive bg-main)
          (border-mode-line-inactive bg-main)))
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-operandi t))

(defun my/fix-menu-face (&rest _)
  "Lucid toolkit menus don't follow the theme unless `menu' is styled."
  (set-face-attribute 'menu nil
                       :background (face-background 'default nil t)
                       :foreground (face-foreground 'default nil t)))
(my/fix-menu-face)
(add-hook 'after-make-frame-functions #'my/fix-menu-face)

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

;; No beacon (cursor trail): it flashed on every point move, which made mouse
;; selection look awful, and its timers spun the CPU in a daemon with tty + GUI
;; frames. pulsar above is the only flash effect.

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
;; Mode line: custom windline-style (flat, thin accent bars)
;; ============================================================
;; Hand-built because every packaged modeline either shares one separator
;; glyph between neighbors (powerline/spaceline: hover can't cover a single
;; segment's edges) or has no separators at all (doom-modeline). Here each
;; segment is its own "thin bar + text" run carrying one `mouse-face', so
;; hovering recolors the bar and the text together - nothing is shared.
;; Foreground-only coloring (no filled backgrounds) is what lets one hover
;; face fit every segment. Colors come from standard faces, so any theme works.
;; which-func is intentionally not shown: breadcrumb-mode's header-line
;; already gives an imenu-based "what scope am I in" trail.
;; winum itself is set up in init.workspace.el (keybindings live there too).

(defun my/number-to-roman (n)
  "Convert N to an uppercase Roman numeral string."
  (if (zerop n)
      "0"
    (let ((table '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
                   (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
                   (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I")))
          (result ""))
      (dolist (pair table result)
        (while (>= n (car pair))
          (setq result (concat result (cdr pair))
                n (- n (car pair))))))))

(defface my/ml-hover
  '((t :inherit (bold font-lock-keyword-face)))
  "Recolor for a whole modeline segment (bar and text) under the mouse.")
(defface my/ml-state-normal '((t :inherit link :underline nil :weight bold)) "Meow normal.")
(defface my/ml-state-insert '((t :inherit success :weight bold)) "Meow insert.")
(defface my/ml-state-motion '((t :inherit warning :weight bold)) "Meow motion.")
(defface my/ml-state-keypad '((t :inherit error :weight bold)) "Meow keypad.")
(defface my/ml-state-beacon '((t :inherit font-lock-keyword-face :weight bold)) "Meow beacon.")

(defun my/ml-state-face ()
  (if (bound-and-true-p meow-mode)
      (pcase meow--current-state
        ('insert 'my/ml-state-insert)
        ('motion 'my/ml-state-motion)
        ('keypad 'my/ml-state-keypad)
        ('beacon 'my/ml-state-beacon)
        (_ 'my/ml-state-normal))
    'shadow))

(defun my/ml-bg (active)
  "Modeline background color, for hiding text on it."
  (let ((bg (face-background (if active 'mode-line-active 'mode-line-inactive) nil t)))
    (if (and (stringp bg) (not (string-prefix-p "unspecified" bg)))
        bg
      (if (eq (frame-parameter nil 'background-mode) 'dark) "#000000" "#ffffff"))))

(defun my/ml-seg (text bar-face &optional text-face props)
  "One segment: a thin BAR-FACE bar plus TEXT, hovered as a single unit.
Like the brackets on becker.kiwi links, [ and ] sit in the segment's edge
cells in the background color (invisible, nothing shifts) and the hover face
recolors them into view. TEXT may carry its own faces; TEXT-FACE only fills
in where it has none."
  (let* ((active (mode-line-window-selected-p))
         (hide (list :foreground (my/ml-bg active)))
         (body (copy-sequence text)))
    (cond ((not active) (put-text-property 0 (length body) 'face 'shadow body))
          (text-face (add-face-text-property 0 (length body) text-face t body)))
    (let ((s (concat (propertize "▎" 'face (if active bar-face 'shadow))
                     (propertize "[" 'face hide)
                     body
                     (propertize "]" 'face hide))))
      (add-text-properties 0 (length s) (append '(mouse-face my/ml-hover) props) s)
      ;; Plain gap outside the hover run: neighbors sharing one mouse-face
      ;; symbol would otherwise highlight together as a single region.
      (concat s " "))))

(defvar my/ml-buffer-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'mode-line-previous-buffer)
    (define-key m [mode-line mouse-3] #'mode-line-next-buffer)
    m))

(defvar my/ml-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line down-mouse-1]
                `(menu-item "Menu Bar" ignore
                            :filter ,(lambda (_) (mouse-menu-major-mode-map))))
    (define-key m [mode-line mouse-2] #'describe-mode)
    m))

(defun my/ml-vcs-click (event)
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (magit-status)))

(defvar my/ml-vcs-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'my/ml-vcs-click)
    m))

(defun my/ml-window-number ()
  (if (bound-and-true-p winum-mode)
      (my/number-to-roman (winum-get-number))
    ""))

(defun my/ml-state-label ()
  (if (bound-and-true-p meow-mode)
      (pcase meow--current-state
        ('insert "INS") ('motion "MOT") ('keypad "KEY") ('beacon "BCN") (_ "NOR"))
    ""))

(defun my/ml-diagnostics ()
  "Flymake error/warning counts, nil when there are none."
  (when (bound-and-true-p flymake-mode)
    (ignore-errors
      (let ((err 0) (warn 0))
        (dolist (d (flymake-diagnostics))
          (let ((sev (flymake--severity (flymake-diagnostic-type d))))
            (cond ((>= sev (flymake--severity :error)) (cl-incf err))
                  ((>= sev (flymake--severity :warning)) (cl-incf warn)))))
        (when (> (+ err warn) 0)
          (concat (when (> err 0) (propertize (format "✖%d" err) 'face 'error))
                  (when (and (> err 0) (> warn 0)) " ")
                  (when (> warn 0) (propertize (format "▲%d" warn) 'face 'warning))))))))

(defun my/ml-minor-modes ()
  "Names of the active minor modes in the current buffer."
  (let (names)
    (dolist (m minor-mode-alist)
      (let ((sym (car m)))
        (when (and (boundp sym) (symbol-value sym)
                   (not (memq sym '(semantic-minor-modes-format override-global-mode))))
          (push (replace-regexp-in-string "-mode\\'" "" (symbol-name sym)) names))))
    (nreverse names)))

(defvar my/ml-minor-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line down-mouse-1] mode-line-mode-menu)
    m))

(defun my/ml-project ()
  (when (and (bound-and-true-p projectile-mode)
             (not (file-remote-p default-directory))
             (projectile-project-p))
    (projectile-project-name)))

(defun my/ml-encoding ()
  "Coding system name, only when it is not plain UTF-8."
  (let ((cs (symbol-name (or buffer-file-coding-system 'utf-8))))
    (unless (string-match-p "\\`\\(utf-8\\|undecided\\|prefer-utf-8\\)" cs)
      cs)))

(defun my/ml-left ()
  (let ((state (my/ml-state-label)))
    (concat
     (my/ml-seg (string-trim (concat (my/ml-window-number) " " state))
                (my/ml-state-face) 'bold)
     (my/ml-seg (concat (cond (buffer-read-only "\ue0a2 ")
                              ((and buffer-file-name (buffer-modified-p)) "● "))
                        (buffer-name))
                'my/ml-state-normal 'bold
                (list 'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                      'local-map my/ml-buffer-map))
     (my/ml-seg (format-mode-line "%l:%c %p") 'shadow)
     (when-let* ((proj (my/ml-project)))
       (my/ml-seg proj 'shadow))
     (when-let* ((diag (my/ml-diagnostics)))
       (my/ml-seg diag 'error)))))

(defun my/ml-right ()
  (concat
   (when defining-kbd-macro (my/ml-seg "REC" 'error 'error))
   (when (use-region-p)
     (my/ml-seg (format "%d sel" (abs (- (region-end) (region-beginning)))) 'warning))
   (when (buffer-narrowed-p) (my/ml-seg "narrow" 'warning))
   (when-let* ((host (file-remote-p default-directory 'host)))
     (my/ml-seg (concat "@" host) 'warning 'warning))
   (when-let* ((proc (and mode-line-process
                          (string-trim (format-mode-line mode-line-process))))
               (_ (not (string-empty-p proc))))
     (my/ml-seg proc 'shadow))
   (when-let* ((enc (my/ml-encoding)))
     (my/ml-seg enc 'warning))
   (when-let* ((clock (bound-and-true-p org-mode-line-string))
               (_ (not (string-empty-p (string-trim (substring-no-properties clock))))))
     (my/ml-seg (string-trim (substring-no-properties clock)) 'warning))
   (when (bound-and-true-p eglot--managed-mode)
     (my/ml-seg "LSP" 'success))
   (when-let* ((minors (my/ml-minor-modes)))
     (my/ml-seg (format "⚙ %d" (length minors)) 'shadow nil
                (list 'help-echo (concat "Minor modes:\n" (string-join minors ", ")
                                         "\nmouse-1: toggle menu")
                      'local-map my/ml-minor-map)))
   (when (and vc-mode buffer-file-name)
     (my/ml-seg (concat "\ue0a0 " (replace-regexp-in-string
                                   "^ *[A-Za-z]+[-:]" "" (substring-no-properties vc-mode)))
                'success nil
                (list 'help-echo "mouse-1: Magit status" 'local-map my/ml-vcs-map)))
   (my/ml-seg (substring-no-properties (format-mode-line mode-name))
              'font-lock-keyword-face nil
              (list 'help-echo "Major mode\nmouse-1: Mode menu\nmouse-2: Describe mode"
                    'local-map my/ml-mode-map))
   (my/ml-seg (format-time-string "%H:%M") 'shadow)))

(defun my/ml-render ()
  (let ((right (my/ml-right)))
    (concat (my/ml-left)
            (propertize " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(string-width right)))))
            right)))

(setq-default mode-line-format '(:eval (my/ml-render)))

;; `highlight' face (hover on buttons/dired/completion): theme's dark
;; foreground is illegible on its own blue background.
(defun my/fix-highlight-face (&rest _)
  (set-face-attribute 'highlight nil :foreground "#ffffff"))
(my/fix-highlight-face)
(add-hook 'after-make-frame-functions #'my/fix-highlight-face)

;; Keeps the modeline clock ticking (`my/ml-right' formats the time itself).
(use-package time
  :ensure nil
  :init
  (setq display-time-format "%H:%M"
        display-time-default-load-average nil)
  (display-time-mode 1))

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
                             "\\\\" "://"
                             "/\\" "=:" "\\/" "tt" "!!!" "&&&" "..=" "/**"
                             ";;;" "|->" "|||"
                             "!~" "&=" ":?" "=~" "&&=" "//=" ":?>" "<-|" "<:<" "<=|"
                             "=<=" "=>=" "??=" "???" "|=>" "||-" "--}}" "<#--" "{{--"
                             "<!---" "Ef" "Et" "Hf" "Ht" "ff" "fff"
                             "th" "fi" "fl" "ft" "ffi" "ffl" "ri")))

;; mode-line


;; page-break-lines-mode
(use-package page-break-lines
  :if (display-graphic-p))

;; ============================================================
;; Tab Line - built-in buffer tabs
;; ============================================================
;; spacious-padding pads each tab with a :box in the tab's own background
;; color, but `tab-line-highlight' has no box - hover then filled only the
;; text area and left the padding ring in the old color. Give it a box in
;; its own background so the whole tab fills.
(defun my/fix-tab-line-highlight (&rest _)
  (when (and (facep 'tab-line-highlight) (fboundp 'modus-themes-get-color-value))
    (let ((bg (modus-themes-get-color-value 'bg-hover))
          (w (or (plist-get (bound-and-true-p spacious-padding-widths) :tab-width) 4)))
      (set-face-attribute 'tab-line-highlight nil
                           :box `(:line-width ,w :color ,bg :style nil)))))
(with-eval-after-load 'tab-line (my/fix-tab-line-highlight))
(dolist (hook '(enable-theme-functions after-make-frame-functions))
  (add-hook hook #'my/fix-tab-line-highlight))

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
;; Terminal: always the dark theme, on the terminal's own background
;; ============================================================
(defun my/terminal-dark-faces (frame)
  "Paint tty FRAME with `modus-vivendi', whatever theme GUI frames use.
Themes are global, faces are not: the dark theme is loaded without enabling
it and its face specs are applied to FRAME only. The kitty window is dark,
so the light theme there means black text on black and a light-gray mode
line. Enabling or disabling a theme recalculates every frame's faces, so this
also runs from the theme hooks below. \"unspecified-bg\" is the Emacs sentinel for
\"use the terminal's own background\"; the symbol `unspecified' would only
inherit."
  (unless (get 'modus-vivendi 'theme-settings)
    (load-theme 'modus-vivendi t t))
  (dolist (entry (get 'modus-vivendi 'theme-settings))
    (when (eq (car entry) 'theme-face)
      (let ((face (nth 1 entry)))
        (face-spec-reset-face face frame)
        (face-spec-set-2 face frame (face-spec-choose (nth 3 entry) frame)))))
  (set-face-attribute 'default frame :background "unspecified-bg")
  (set-frame-parameter frame 'background-mode 'dark)
  ;; Modus fakes the mode line border on displays without :box support with
  ;; an underline; a tty draws that under the text instead.
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (set-face-attribute face frame :underline nil :box nil)))

(defun my/terminal-refresh-faces (&rest _)
  (dolist (frame (frame-list))
    (when (and (eq (framep frame) t) (frame-parameter frame 'my/tty-dark))
      (my/terminal-dark-faces frame))))
;; Disabling the old theme recalculates every frame's faces too, after the
;; new one was enabled, so both ends (and modus' own final hook) re-apply.
(dolist (hook '(enable-theme-functions disable-theme-functions
                modus-themes-after-load-theme-hook))
  (add-hook hook #'my/terminal-refresh-faces))

(defun my/terminal-setup ()
  "Configure Emacs for terminal frames: dark faces on the terminal's background."
  (unless (display-graphic-p)
    (set-frame-parameter nil 'my/tty-dark t)
    (my/terminal-dark-faces (selected-frame))
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
The graphical branch matches the frame chrome (visible briefly before
the theme paints the buffer) to the current theme's actual colors, read
live rather than hardcoded - stays correct across theme switches.
Both foreground and background: `early-init.el' hard-codes both in
`default-frame-alist' (white-on-black, to avoid a flash-of-white on the
old dark theme) as the very first values a new frame gets, before any
theme loads. A frame's `default' face background does get corrected by
the theme itself once it paints, but the *foreground* half of that
stale pair doesn't reliably get overridden the same way - faces that
plainly inherit `default' (like `my/dashboard-item') were rendering
white-on-light and unreadable under doom-earl-grey until this also
set foreground-color explicitly, not just background-color."
  (if (frame-parameter frame 'window-system)
      (progn
        (set-frame-parameter frame 'background-color
                              (face-background 'default frame t))
        (set-frame-parameter frame 'foreground-color
                              (face-foreground 'default frame t))
        (force-mode-line-update t))
    ;; Terminal frame: dark faces for this frame only (FRAME, never nil: a nil
    ;; frame would change every existing frame, GUI included).
    (with-selected-frame frame
      (my/terminal-setup)
      (force-mode-line-update t))))

;; Fires for every new frame: daemon emacsclient connections and
;; additional frames in a running session.
(add-hook 'after-make-frame-functions #'my/setup-frame)

;; No daemon theme-reload hook here (there was one): it existed only
;; because spacemacs-theme/humanoid-themes baked truecolor-vs-fallback
;; detection into their faces at load time, which breaks with zero frames
;; at daemon startup. modus-themes does no such detection - its colors are
;; correct immediately, daemon or not - so the reload was dead weight.

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
