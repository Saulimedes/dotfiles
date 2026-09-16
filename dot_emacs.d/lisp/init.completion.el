;; -*- lexical-binding: t; -*-
;; Modern completion stack: Vertico + Consult + Corfu + Cape

;; ============================================================
;; Minibuffer Completion: Vertico
;; ============================================================

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-l" . vertico-insert)
              ("C-u" . vertico-scroll-down)
              ("C-d" . vertico-scroll-up)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)
  (vertico-count 12)
  (vertico-resize nil)
  (vertico-scroll-margin 2)
  :init
  (vertico-mode)
  ;; Vertico extensions
  (vertico-multiform-mode)
  :config
  ;; Different display for different commands
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer)
          (consult-git-grep buffer)
          (consult-grep buffer)
          (consult-imenu buffer)
          (consult-outline buffer))))

;; Directory navigation extension
(use-package vertico-directory
  :after vertico
  :ensure nil
  :straight nil
  :load-path "straight/build/vertico/extensions/"
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; ============================================================
;; Fuzzy Matching: Orderless
;; ============================================================

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal
                               orderless-prefixes
                               orderless-initialism
                               orderless-regexp)))

;; ============================================================
;; Rich Annotations: Marginalia
;; ============================================================

(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :init
  (marginalia-mode))

;; ============================================================
;; Enhanced Commands: Consult
;; ============================================================

(use-package consult
  :demand t
  :bind (;; C-c bindings
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; M-g bindings (goto)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Other
         ("C-s" . consult-line)
         ("M-y" . consult-yank-pop))
  :custom
  (consult-narrow-key "<")
  (consult-project-root-function #'project-root)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Preview on any key
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; ============================================================
;; Context Actions: Embark
;; ============================================================

(straight-use-package 'embark-consult)

(use-package embark
  :demand t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================
;; In-Buffer Completion: Corfu
;; ============================================================

(use-package corfu
  :demand t
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-l" . corfu-insert)
              ("C-g" . corfu-quit)
              ("M-d" . corfu-popupinfo-toggle)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; Completion sources
(use-package cape
  :demand t
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; Tmux pane completion - complete words visible in other tmux panes
(defun my/tmux-live-p ()
  "Return non-nil if a tmux server is reachable.
Checking `getenv' for \"TMUX\" is unreliable under the Emacs
daemon: that reflects whatever environment the long-lived daemon
process happened to inherit at its own startup, not the tmux pane
you are actually connecting from with emacsclient right now.
Probing the server directly works regardless of which process (or
which pane) is asking."
  (and (executable-find "tmux")
       (= 0 (call-process "tmux" nil nil nil "list-sessions"))))

(defun my/tmux-pane-words ()
  "Collect words from all panes across all tmux sessions."
  (when (my/tmux-live-p)
    (let ((words '()))
      (dolist (pane (split-string
                     (shell-command-to-string
                      "tmux list-panes -a -F '#{pane_id}'") "\n" t))
        (let ((text (shell-command-to-string
                     (format "tmux capture-pane -t %s -p" pane))))
          (dolist (word (split-string text "[^a-zA-Z0-9_.-]+" t))
            (when (>= (length word) 3)
              (push word words)))))
      (delete-dups words))))

(defun my/cape-tmux ()
  "Completion-at-point function for tmux pane content."
  (when (my/tmux-live-p)
    (let ((bounds (cape--bounds 'word)))
      (when bounds
        `(,(car bounds) ,(cdr bounds)
          ,(completion-table-dynamic
            (lambda (_) (my/tmux-pane-words)))
          :exclusive no)))))

;; Many major modes set completion-at-point-functions buffer-locally,
;; replacing rather than extending the global default - so adding cape
;; sources to the default value (as above, via :init/add-to-list) never
;; actually reaches real file buffers. after-change-major-mode-hook runs
;; after the major mode (and its own CAPF setup) is fully done, so
;; appending here, buffer-locally, is what actually works.
(defvar-local my/cape-extra-capfs-added nil
  "Non-nil once `my/cape-add-extra-capfs' has run in this buffer.
Both wrapping an already-wrapped capf again and re-adding cape-file
etc. as fresh entries are silently harmless-looking but compound on
every re-run (after-change-major-mode-hook can fire more than once
for the same buffer), so this must be idempotent.")

(defun my/cape-add-extra-capfs ()
  (unless my/cape-extra-capfs-added
    (setq my/cape-extra-capfs-added t)
    ;; The mode's own capf (e.g. elisp-completion-at-point) claims the
    ;; position exclusively by default: if it has zero matching candidates
    ;; for what you typed, completion stops right there instead of trying
    ;; the sources below at all. cape-wrap-nonexclusive fixes that - but it
    ;; calls its argument immediately (see its docstring: "Call CAPF..."),
    ;; so it must be applied as :around advice on the existing function
    ;; (lazy, invoked later whenever that capf actually runs), never
    ;; mapped directly over the capf list, which would call each capf
    ;; right now, at setup time, and store whatever one-off result that
    ;; happened to produce - including nil - as a permanent list entry.
    (dolist (f completion-at-point-functions)
      (when (and (functionp f)
                 (not (advice-member-p #'cape-wrap-nonexclusive f)))
        (advice-add f :around #'cape-wrap-nonexclusive)))
    (add-hook 'completion-at-point-functions #'cape-file 90 t)
    (add-hook 'completion-at-point-functions #'cape-dabbrev 90 t)
    (add-hook 'completion-at-point-functions #'my/cape-tmux 90 t)))
(add-hook 'after-change-major-mode-hook #'my/cape-add-extra-capfs)

;; *scratch* is created by Emacs itself at bootstrap, before this file (and
;; the hook above) ever loads, so it never gets the hook treatment on any
;; startup. Apply it here, once, directly.
(when (get-buffer "*scratch*")
  (with-current-buffer "*scratch*"
    (my/cape-add-extra-capfs)))

;; Icons for corfu
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ============================================================
;; Better Help
;; ============================================================

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-h o" . helpful-symbol)
         ("C-c C-d" . helpful-at-point)))

;; ============================================================
;; Recent Files
;; ============================================================

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode))

;; ============================================================
;; Consult-eglot - LSP symbol search via consult
;; ============================================================
(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-g s" . consult-eglot-symbols)))

;; ============================================================
;; Snippets: YASnippet
;; ============================================================
(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (add-to-list 'yas-snippet-dirs yasnippet-snippets-dir t)
  (yas-reload-all))

;; ============================================================
;; Wgrep - Writable grep results
;; ============================================================
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; ============================================================
;; Consult-dir - Quick directory switching
;; ============================================================
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(provide 'init.completion)
