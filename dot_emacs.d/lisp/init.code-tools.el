;; Advanced code navigation and editing tools

;; Jump to definition across files
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

;; Smart code folding
(use-package origami
  :hook (prog-mode . origami-mode)
  :config
  (global-origami-mode))

;; Quick edit similar items
(use-package iedit
  :bind ("C-;" . iedit-mode))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-c C-SPC" . mc/edit-lines)))

;; Code structure outline with imenu-list
(use-package imenu-list
  :bind ("C-'" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

;; Automatic code formatting
(use-package format-all
  :hook (prog-mode . format-all-mode)
  :custom
  (format-all-formatters
   '(("Python" black)
     ("JavaScript" prettier)
     ("TypeScript" prettier)
     ("CSS" prettier)
     ("Rust" rustfmt)
     ("Go" gofmt)
     ("Markdown" prettier)
     ("YAML" prettier))))

;; Rainbow delimiters for better code readability
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Editorconfig support
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Better commented code highlighting
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF4500")
          ("DEBUG"  . "#A020F0")
          ("NOTE"   . "#1E90FF")
          ("HACK"   . "#FFA500")
          ("BUG"    . "#FF0000")
          ("XXX"    . "#1E90FF"))))

;; Move text easily
(use-package move-text
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down)))

;; Keybindings for code tools (Meow compatible)
(global-set-key (kbd "C-c z f") 'origami-close-node)
(global-set-key (kbd "C-c z o") 'origami-open-node)
(global-set-key (kbd "C-c z r") 'origami-open-all-nodes)
(global-set-key (kbd "C-c z m") 'origami-close-all-nodes)
(global-set-key (kbd "C-c z t") 'origami-toggle-node)
(global-set-key (kbd "C-c c d") 'xref-find-definitions)
(global-set-key (kbd "C-c c r") 'xref-find-references)
(global-set-key (kbd "C-c c i") 'imenu-list-smart-toggle)
(global-set-key (kbd "C-c c e") 'iedit-mode)
(global-set-key (kbd "C-c c f") 'format-all-buffer)
(global-set-key (kbd "C-c x e") 'eval-last-sexp)
(global-set-key (kbd "C-c x b") 'eval-buffer)
(global-set-key (kbd "C-c x f") 'eval-defun)

(provide 'init.code-tools)