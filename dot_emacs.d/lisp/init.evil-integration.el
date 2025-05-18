;; Improved evil integrations with various modes

;; Evil for Org Mode
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Evil for Magit
(use-package evil-magit
  :after (evil magit)
  :config
  (evil-magit-init))

;; Evil text objects for pairs
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; Evil Lion - Align text with gl and gL
(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

;; Evil Visualstar - * and # search for visual selections
(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))

;; Evil exchange - exchange text with gx
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

;; Evil numbers - increment/decrement numbers with C-a and C-x
(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

;; Evil-snipe - faster f/t motions with s/S
(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  ;; Use vim-seek behavior
  (setq evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-smart-case t))

;; Ex command aliases for common operations
(evil-ex-define-cmd "W" 'save-buffer)  ; Allow :W to save like :w
(evil-ex-define-cmd "Q" 'kill-buffer)  ; Allow :Q to quit like :q
(evil-ex-define-cmd "Wq" 'evil-save-and-close) ; Allow :Wq to save and quit
(evil-ex-define-cmd "WQ" 'evil-save-and-close) ; Allow :WQ to save and quit

;; Evil-goggles - visual hint on edit operations
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.150))

;; Enable persistent undo history with undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist 
        `(("." . ,(expand-file-name "undo-tree-history" emacs-cache-directory))))
  ;; Make Evil use undo-tree for better undo behavior
  (evil-set-undo-system 'undo-tree))

(provide 'init.evil-integration)