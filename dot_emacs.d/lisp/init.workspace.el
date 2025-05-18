;; Persistent workspace management
;; Save and restore window configurations and buffers

;; Tab-bar for workspaces
(use-package tab-bar
  :ensure nil  ; Built into Emacs 27+
  :custom
  (tab-bar-show 1)  ; Always show the tab bar
  (tab-bar-tab-hints t)  ; Show numeric hints
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-button-show nil)  ; No close button
  (tab-bar-new-button-show nil)    ; No new button
  :config
  (tab-bar-mode 1)
  
  ;; Initialize default workspaces
  (tab-bar-new-tab)
  (tab-bar-rename-tab "code")
  (tab-bar-new-tab)
  (tab-bar-rename-tab "org"))

;; Better keybindings for tab-bar
(global-set-key (kbd "C-c w r") 'tab-bar-rename-tab)
(global-set-key (kbd "C-c w n") 'tab-bar-new-tab)
(global-set-key (kbd "C-c w k") 'tab-bar-close-tab)
(global-set-key (kbd "C-c w p") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-c w n") 'tab-bar-switch-to-next-tab)

;; Desktop save mode - Save session state
(use-package desktop
  :ensure nil
  :custom
  (desktop-save t)  ; Always save
  (desktop-load-locked-desktop t)  ; Load even if locked
  (desktop-path (list (expand-file-name "desktop" emacs-cache-directory)))
  (desktop-restore-frames t)  ; Restore frame configuration
  (desktop-restore-window-configuration t)  ; Restore window configuration
  :config
  (make-directory (expand-file-name "desktop" emacs-cache-directory) t)
  (desktop-save-mode 1))

;; Window management with winner-mode
(use-package winner
  :ensure nil
  :custom
  (winner-boring-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*" "*Apropos*" "*Help*"))
  :config
  (winner-mode 1))

;; Advanced window numbering
(use-package winum
  :config
  (winum-mode))

;; Window management keybindings
(with-eval-after-load 'evil
  ;; Evil keys for window navigation enhanced with winum
  (evil-define-key 'normal 'global
    (kbd "<leader>w1") 'winum-select-window-1
    (kbd "<leader>w2") 'winum-select-window-2
    (kbd "<leader>w3") 'winum-select-window-3
    (kbd "<leader>w4") 'winum-select-window-4
    (kbd "<leader>w5") 'winum-select-window-5
    
    ;; Tab bar workspace management
    (kbd "<leader>wt") '(:ignore t :which-key "tabs")
    (kbd "<leader>wtr") 'tab-bar-rename-tab
    (kbd "<leader>wtn") 'tab-bar-new-tab
    (kbd "<leader>wtx") 'tab-bar-close-tab
    (kbd "<leader>wts") 'tab-bar-select-tab-by-name
    (kbd "<leader>wtp") 'tab-bar-switch-to-prev-tab
    (kbd "<leader>wtn") 'tab-bar-switch-to-next-tab
    
    ;; Window history with winner-mode
    (kbd "<leader>wu") 'winner-undo
    (kbd "<leader>wr") 'winner-redo
    
    ;; Window splits
    (kbd "<leader>wv") 'split-window-right
    (kbd "<leader>ws") 'split-window-below
    (kbd "<leader>wd") 'delete-window
    (kbd "<leader>wo") 'delete-other-windows
    
    ;; Balance windows
    (kbd "<leader>w=") 'balance-windows)
  
  ;; Update which-key descriptions for workspace commands
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "SPC w" "windows/workspaces")
    (which-key-add-key-based-replacements "SPC wt" "tabs")))

(provide 'init.workspace)