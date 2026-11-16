;; -*- lexical-binding: t; -*-
;; Better TAB key handling and autocompletion by filetype

;; TAB: indent first, complete when already indented (Corfu popup)
(setq tab-always-indent 'complete
      tab-first-completion 'word-or-paren-or-punct)

(defvar my/indent-significant-modes
  '(yaml-mode yaml-ts-mode python-base-mode)
  "Modes where indentation is structure, not style.
`indent-region' re-derives every line's column from the line above, which
flattens a pasted or selected block in these modes, so TAB and S-TAB on a
region move the whole block by one level instead.")

(defun my/indent-significant-p ()
  (apply #'derived-mode-p my/indent-significant-modes))

(defun my/indent-step ()
  "Columns one indentation level is worth in this buffer."
  (cond ((derived-mode-p 'yaml-mode 'yaml-ts-mode)
         (if (boundp 'yaml-indent-offset) yaml-indent-offset 2))
        ((derived-mode-p 'python-base-mode) python-indent-offset)
        (t tab-width)))

(defun my/shift-lines (beg end levels)
  "Shift the lines from BEG to END by LEVELS indentation levels.
Whole lines only (BEG is moved back to the start of its line), blank lines
are left alone, and the region stays active so the key can be repeated."
  (let ((beg (save-excursion (goto-char beg) (line-beginning-position))))
    (indent-rigidly beg end (* levels (my/indent-step)))))

(defun my/tab-dwim ()
  "Do-what-I-mean for TAB.
- Region active: shift the block one level in indentation-significant modes
  (yaml, python), re-indent it by syntax everywhere else.
- Read-only buffer (dired, elfeed, special modes...): jump to the next
  button/link instead of erroring, or say there is nothing to do.
- Otherwise: `indent-for-tab-command', which indents to the mode's correct
  column (spaces, never a hard tab: python/yaml/lisp all break on those) and,
  because `tab-always-indent' is `complete', completes when already indented.
  yaml-mode cycles deeper-to-shallower on repeated TAB."
  (interactive)
  (cond
   ((use-region-p)
    (if (my/indent-significant-p)
        (my/shift-lines (region-beginning) (region-end) 1)
      (indent-region (region-beginning) (region-end))))
   (buffer-read-only
    (condition-case nil
        (forward-button 1)
      (error (message "TAB: no buttons or links in this read-only buffer"))))
   (t
    (indent-for-tab-command))))

;; Global fallback; modes with their own TAB (org, magit, dirvish, help,
;; vterm, eshell, corfu popup, minibuffer) keep theirs because their keymaps
;; take precedence over this one.
(global-set-key (kbd "TAB") 'my/tab-dwim)

(defun my/backtab-dwim ()
  "Shift+TAB: previous button in read-only buffers, otherwise dedent.
Dedents by one indentation level in yaml/python, by a tab stop elsewhere."
  (interactive)
  (cond
   (buffer-read-only
    (condition-case nil
        (backward-button 1)
      (error (message "S-TAB: no buttons or links in this read-only buffer"))))
   (t
    (let ((beg (if (use-region-p) (region-beginning) (line-beginning-position)))
          (end (if (use-region-p) (region-end) (line-end-position))))
      (if (my/indent-significant-p)
          (my/shift-lines beg end -1)
        (indent-rigidly-left-to-tab-stop beg end))))))

(global-set-key (kbd "<backtab>") 'my/backtab-dwim)

;; Filetype-specific indentation settings
(setq-default indent-tabs-mode nil   ; Use spaces by default
              tab-width 4)           ; Default tab width

;; Programming language specific settings
(use-package prog-mode
  :ensure nil
  :straight nil
  :config
  (defun my/set-up-prog-mode ()
    "Set up indentation for various programming modes."
    (setq tab-width 4))
  
  (add-hook 'prog-mode-hook 'my/set-up-prog-mode))

;; Python settings
(use-package python
  :ensure nil
  :config
  (setq python-indent-offset 4))

;; JavaScript/TypeScript/JSON settings
(use-package js
  :ensure nil
  :config
  (setq js-indent-level 2))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))

;; Web development modes
(use-package web-mode
  :mode ("\\.html?\\'" "\\.vue\\'" "\\.svelte\\'"
         "\\.php\\'" "\\.erb\\'" "\\.hbs\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  :config
  ;; Auto-close tags
  (setq web-mode-auto-close-style 2))

;; CSS / SCSS / LESS
(use-package css-mode
  :ensure nil
  :mode ("\\.css\\'" "\\.less\\'")
  :custom
  (css-indent-offset 2))

(use-package scss-mode
  :mode "\\.scss\\'"
  :custom
  (scss-compile-at-save nil))

;; Emmet - expand abbreviations to HTML/CSS
;; e.g. div.container>ul>li.item*3 + C-j = full HTML
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)
         (scss-mode . emmet-mode)
         (html-mode . emmet-mode))
  :custom
  (emmet-indentation 2)
  (emmet-move-cursor-between-quotes t))

;; C/C++ settings
(use-package cc-mode
  :ensure nil
  :config
  (setq c-basic-offset 4))

;; Yaml settings
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (setq yaml-indent-offset 2))

;; Aggressive indentation for some modes
(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-mode) . aggressive-indent-mode))

(provide 'init.tab-config)