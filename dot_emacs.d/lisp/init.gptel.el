;; -*- lexical-binding: t; -*-
;; gptel - LLM assistant, backed by Anthropic Claude. API key comes from the
;; pass store via auth-source-pass (see init.code-tools.el): add it with
;; `pass insert api.anthropic.com' (host must match exactly, gptel looks up
;; by host "api.anthropic.com" / user "apikey").

(use-package gptel
  :commands (gptel gptel-rewrite my/gptel-add-comments my/gptel-explain-code)
  :config
  (setq gptel-backend
        (gptel-make-anthropic "Claude" :stream t :models '(claude-sonnet-5))
        gptel-model 'claude-sonnet-5))

(defun my/gptel-add-comments (beg end)
  "Ask gptel to add explanatory comments to the code between BEG and END.
Replaces the region in place - review with `undo' if the result is off."
  (interactive "r")
  (unless (use-region-p)
    (user-error "Select a region first"))
  (let* ((code (buffer-substring-no-properties beg end))
         (buf (current-buffer))
         (start (copy-marker beg))
         (end (copy-marker end)))
    (message "Asking %s for comments..." (gptel-backend-name gptel-backend))
    (gptel-request
        (format "Add concise, accurate comments to the code below, \
explaining anything non-obvious. Do not change the code logic or \
formatting beyond adding comments. Return only the resulting code, \
no markdown fences, no commentary.\n\n%s" code)
      :system (or (alist-get 'programming gptel-directives) gptel-system-prompt)
      :callback
      (lambda (response info)
        (if (stringp response)
            (with-current-buffer buf
              (save-excursion
                (delete-region start end)
                (goto-char start)
                (insert (string-trim response "```[a-z]*\n?" "\n?```")))
              (undo-boundary)
              (message "Comments added."))
          (message "gptel comment request failed: %s" (plist-get info :status)))))))

(defvar my/gptel-explain-buffer-name "*gptel-explain*")

(defun my/gptel-explain-code (beg end)
  "Ask gptel to explain the code between BEG and END in a separate buffer."
  (interactive "r")
  (unless (use-region-p)
    (user-error "Select a region first"))
  (let ((code (buffer-substring-no-properties beg end)))
    (message "Asking %s to explain..." (gptel-backend-name gptel-backend))
    (gptel-request
        (format "Explain what the following code does, concisely but \
completely. Assume the reader knows the language but not this specific \
code.\n\n%s" code)
      :system (or (alist-get 'programming gptel-directives) gptel-system-prompt)
      :callback
      (lambda (response info)
        (if (stringp response)
            (with-current-buffer (get-buffer-create my/gptel-explain-buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert response)
                (goto-char (point-min)))
              (if (fboundp 'gfm-mode) (gfm-mode) (text-mode))
              (visual-line-mode 1)
              (view-mode 1)
              (display-buffer (current-buffer)))
          (message "gptel explain request failed: %s" (plist-get info :status)))))))

;; gptel-commit - Magit integration for generating commit messages from the
;; staged diff, via the same gptel backend/model configured above.
(use-package gptel-commit
  :after (gptel magit)
  :custom (gptel-commit-stream t))

(with-eval-after-load 'magit
  (define-key git-commit-mode-map (kbd "C-c g") #'gptel-commit)
  (define-key git-commit-mode-map (kbd "C-c G") #'gptel-commit-rationale))

(global-set-key (kbd "C-c A g") #'gptel)
(global-set-key (kbd "C-c A r") #'gptel-rewrite)
(global-set-key (kbd "C-c A c") #'my/gptel-add-comments)
(global-set-key (kbd "C-c A e") #'my/gptel-explain-code)

(provide 'init.gptel)
