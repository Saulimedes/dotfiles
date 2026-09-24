;; -*- lexical-binding: t; -*-
;; sidebar (treemacs; see init.projectile.el for treemacs-projectile)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package treemacs
  :bind ("C-c S" . treemacs))

;; dired-open - RET on a file opens it externally instead of visiting it as
;; an Emacs buffer, when the extension matches. Audio/video go to mpv;
;; anything else not covered falls through to xdg-open (dired-open-xdg),
;; which was missing entirely before - dired had no external-open path at
;; all, hence media files just opening as raw buffers in Emacs.
(use-package dired-open
  :after dired
  :custom
  (dired-open-extensions '(("m4b" . "mpv")
                            ("mp3" . "mpv")
                            ("m4a" . "mpv")
                            ("flac" . "mpv")
                            ("ogg" . "mpv")
                            ("opus" . "mpv")
                            ("wav" . "mpv")
                            ("mp4" . "mpv")
                            ("mkv" . "mpv")
                            ("webm" . "mpv")
                            ("avi" . "mpv")
                            ("mov" . "mpv")))
  (dired-open-functions '(dired-open-by-extension dired-open-subdir dired-open-xdg)))

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
    (when-let* ((file (frame-parameter frame 'my/dired-exit-file)))
      (with-selected-frame frame
        (let ((dir (if (derived-mode-p 'dired-mode)
                       (expand-file-name dired-directory)
                     (expand-file-name default-directory))))
          (write-region dir nil file nil 'quiet))))))

(provide 'init.dired)
