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
(global-set-key (kbd "C-c w ]") 'tab-bar-switch-to-next-tab)

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

;; Window management keybindings (Meow compatible)
(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)
(global-set-key (kbd "C-c w u") 'winner-undo)
(global-set-key (kbd "C-c w R") 'winner-redo)
(global-set-key (kbd "C-c w v") 'split-window-right)
(global-set-key (kbd "C-c w s") 'split-window-below)
(global-set-key (kbd "C-c w d") 'delete-window)
(global-set-key (kbd "C-c w o") 'delete-other-windows)
(global-set-key (kbd "C-c w =") 'balance-windows)
(global-set-key (kbd "C-c w t r") 'tab-bar-rename-tab)
(global-set-key (kbd "C-c w t n") 'tab-bar-new-tab)
(global-set-key (kbd "C-c w t x") 'tab-bar-close-tab)
(global-set-key (kbd "C-c w t s") 'tab-bar-select-tab-by-name)

(provide 'init.workspace)