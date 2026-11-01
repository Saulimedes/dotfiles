;; -*- lexical-binding: t; -*-
;; Wallabag - read-it-later client. Credentials come from ~/.authinfo.gpg
;; (wallabag.el's own auth-source lookup, keyed on the literal host
;; "wallabag", not your real domain): add a line there of the form
;; `machine wallabag domain "https://WALLABAG_HOST" user "WALLABAG_USER"
;; password "WALLABAG_PASSWORD" clientid "CLIENT_ID" client-secret
;; "CLIENT_SECRET"' (clientid/client-secret come from Wallabag's API
;; clients management page).

(use-package wallabag
  :commands (wallabag wallabag-find wallabag-add-entry)
  :bind (("C-c R w" . wallabag))
  :custom
  (wallabag-db-file (expand-file-name "wallabag/wallabag.sqlite" emacs-cache-directory))
  (wallabag-search-print-items '("title" "domain" "tag" "reading-time" "date"))
  (wallabag-search-page-max-rows 32))

;; Save the current elfeed entry to Wallabag for later reading.
(with-eval-after-load 'elfeed-show
  (define-key elfeed-show-mode-map (kbd "C-c w") #'wallabag-add-entry))

(provide 'init.wallabag)
