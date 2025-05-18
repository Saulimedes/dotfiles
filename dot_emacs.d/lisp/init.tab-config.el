;; Better TAB key handling and autocompletion by filetype

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Make TAB actually insert a tab character when needed
(defun my/tab-dwim ()
  "Do-what-I-mean for TAB key:
- If region is active, indent the region
- If at beginning of line, insert a tab character
- If point is at a word or after a word, attempt completion
- Otherwise, indent the current line"
  (interactive)
  (cond
   ;; If region is active, indent it
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ;; At beginning of line, insert a real tab
   ((bolp)
    (insert "\t"))
   ;; If we're in a completion context, complete
   ((and (bound-and-true-p corfu-mode)
         (or (looking-at "\\>")
             (looking-back "\\w" 1)))
    (or (corfu-complete)
        (indent-for-tab-command)))
   ;; Otherwise indent
   (t
    (indent-for-tab-command))))

;; Bind TAB key to our smart function and ensure indentation works by file type
(global-set-key (kbd "TAB") 'my/tab-dwim)

;; Filetype-specific indentation settings
(setq-default indent-tabs-mode nil   ; Use spaces by default
              tab-width 4)           ; Default tab width

;; Programming language specific settings
(use-package prog-mode
  :ensure nil
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
  :mode ("\\.html?\\'" "\\.css\\'" "\\.jsx?\\'" "\\.tsx?\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

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

;; Indentation highlighting
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-responsive 'top))

;; Aggressive indentation for some modes
(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-mode) . aggressive-indent-mode))

(provide 'init.tab-config)