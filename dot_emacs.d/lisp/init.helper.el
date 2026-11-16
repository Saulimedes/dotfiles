;; -*- lexical-binding: t; -*-
;; vundo
(use-package vundo
  :config
  (setq vundo-compact-display t))

;; restart emacs
(use-package restart-emacs)

(defun my/kill-buffer-quick ()
  "Kill the current buffer without asking; a modified file is saved first."
  (interactive)
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer))
  (kill-buffer (current-buffer)))

;; Save file buffers when you leave them (buffer/window switch, focus loss)
;; and after 5 s idle, so there is rarely anything left to ask about on close.
(use-package super-save
  :custom
  (super-save-all-buffers t)
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 5)
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-exclude '("\\.gpg\\'"))
  :config
  (super-save-mode 1)
  ;; format-all runs from `before-save-hook': an idle save would reformat code
  ;; under the cursor, so automatic saves skip it (C-x C-s still formats).
  (with-eval-after-load 'format-all
    (define-advice format-all--buffer-from-hook (:around (fn &rest args) skip-on-super-save)
      (unless super-save-in-progress (apply fn args)))))

;; clipmon: syncs system clipboard into kill-ring, browse with M-y
(use-package clipmon
  :config
  (clipmon-mode 1)
  (clipmon-persist))

;; Re-copy a past kill-ring entry to the clipboard, no insert
(defun my/clipboard-select ()
  (interactive)
  (let ((entry (completing-read "Clipboard: " kill-ring nil t)))
    (kill-new entry)
    (message "Copied: %s" (truncate-string-to-width entry 60))))
(global-set-key (kbd "C-c y") #'my/clipboard-select)

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


;; tldr - simplified man pages with examples
(use-package tldr)

(provide 'init.helper)
