;; -*- lexical-binding: t; -*-
;; sidebar
(use-package dired-sidebar
  :bind ("C-c s" . dired-sidebar-toggle-sidebar))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package treemacs
  :bind ("C-c S" . treemacs))

;; Track dired directory for shell cd-on-exit
(defvar my/dired-exit-file-pending nil
  "Temp file path to assign to the next created frame.")

(add-hook 'after-make-frame-functions
  (lambda (frame)
    (when my/dired-exit-file-pending
      (set-frame-parameter frame 'my/dired-exit-file my/dired-exit-file-pending)
      (setq my/dired-exit-file-pending nil))))

(add-hook 'delete-frame-functions
  (lambda (frame)
    (when-let ((file (frame-parameter frame 'my/dired-exit-file)))
      (with-selected-frame frame
        (let ((dir (if (derived-mode-p 'dired-mode)
                       (expand-file-name dired-directory)
                     (expand-file-name default-directory))))
          (write-region dir nil file nil 'quiet))))))

(provide 'init.dired)
