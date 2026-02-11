;; vundo
(use-package vundo
  :config
  (setq vundo-compact-display t))

;; restart emacs
(use-package restart-emacs)

(defun my/kill-buffer-quick ()
  "Kill current buffer without prompt if unmodified, then switch to previous buffer."
  (interactive)
  (if (and (buffer-modified-p) (buffer-file-name))
      (when (y-or-n-p (format "Buffer %s modified. Kill anyway? " (buffer-name)))
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer)))
    (kill-buffer (current-buffer))))

(provide 'init.helper)
