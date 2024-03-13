;; timemachine
(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

;; git-remote
(use-package browse-at-remote
  :bind ("M-g r" . browse-at-remote))

;; magit
(use-package magit
  :bind
  ("M-g s" . magit-status) ; Open Magit status buffer
  ("M-g d" . magit-dispatch) ; Open Magit dispatch popup
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1) ; Show Magit buffers in the same window
  :init
  (setq magit-save-repository-buffers 'dontask) ; Don't ask to save modified buffers when running Magit commands
  (setq magit-repository-directories '(("~/Projects/" . 1))) ; Set the default Magit repository search path (optional)
  (setq magit-auto-revert-mode t)) ; Auto-revert buffers when files change on disk (optional)

(use-package git-messenger
  :bind ("M-g m" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

(use-package git-timemachine
  :bind ("C-x g t" . git-timemachine))


(provide 'init.git)
