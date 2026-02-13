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

;; ============================================================
;; wcopy — watch clipboard and collect entries into a buffer
;; ============================================================

(defvar wcopy--timer nil "Timer for polling the clipboard.")
(defvar wcopy--last nil "Last seen clipboard content.")
(defvar wcopy--count 0 "Number of entries collected.")
(defvar wcopy-buffer-name "*wcopy*" "Buffer name for collected entries.")
(defvar wcopy-poll-interval 1 "Seconds between clipboard polls.")

(defun wcopy--get-clipboard ()
  "Get current system clipboard content via xclip or wl-paste."
  (ignore-errors
    (string-trim
     (shell-command-to-string
      (if (getenv "WAYLAND_DISPLAY")
          "wl-paste --no-newline 2>/dev/null"
        "xclip -selection clipboard -o 2>/dev/null")))))

(defun wcopy--poll ()
  "Check clipboard and append new content to the wcopy buffer."
  (let ((current (wcopy--get-clipboard)))
    (when (and current
               (not (string-blank-p current))
               (not (equal current wcopy--last))
               (let ((buf (get-buffer wcopy-buffer-name)))
                 (not (and buf
                           (with-current-buffer buf
                             (save-excursion
                               (goto-char (point-min))
                               (search-forward current nil t)))))))
      (setq wcopy--last current)
      (cl-incf wcopy--count)
      (with-current-buffer (get-buffer-create wcopy-buffer-name)
        (goto-char (point-max))
        (insert current "\n"))
      (message "[%d] wcopy: %s" wcopy--count
               (truncate-string-to-width current 80)))))

(defun wcopy-toggle ()
  "Toggle clipboard watching on/off."
  (interactive)
  (if wcopy--timer
      (progn
        (cancel-timer wcopy--timer)
        (setq wcopy--timer nil)
        (message "wcopy: off (%d entries)" wcopy--count))
    (setq wcopy--count 0
          wcopy--last (wcopy--get-clipboard))
    (with-current-buffer (get-buffer-create wcopy-buffer-name)
      (erase-buffer))
    (setq wcopy--timer (run-with-timer 0 wcopy-poll-interval #'wcopy--poll))
    (pop-to-buffer wcopy-buffer-name)
    (message "wcopy: on")))

(global-set-key (kbd "C-c w") #'wcopy-toggle)

(provide 'init.helper)
