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

;; Integration with evil mode
(with-eval-after-load 'evil
  ;; Define evil mode keybindings for code tools
  (evil-define-key 'normal 'global
    ;; Code folding with origami
    (kbd "<leader>zf") 'origami-close-node
    (kbd "<leader>zo") 'origami-open-node
    (kbd "<leader>zr") 'origami-open-all-nodes
    (kbd "<leader>zm") 'origami-close-all-nodes
    (kbd "<leader>zt") 'origami-toggle-node
    
    ;; Code navigation
    (kbd "<leader>cd") 'xref-find-definitions
    (kbd "<leader>cr") 'xref-find-references
    (kbd "<leader>ci") 'imenu-list-smart-toggle
    
    ;; Code editing
    (kbd "<leader>ce") 'iedit-mode
    (kbd "<leader>cf") 'format-all-buffer
    
    ;; Code evaluation
    (kbd "<leader>ee") 'eval-last-sexp
    (kbd "<leader>eb") 'eval-buffer
    (kbd "<leader>ef") 'eval-defun)
  
  ;; Update which-key descriptions
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "SPC z" "folding")
    (which-key-add-key-based-replacements "SPC c" "code")
    (which-key-add-key-based-replacements "SPC e" "eval")))

(provide 'init.code-tools)