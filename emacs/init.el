;; Performance
(setq gc-cons-threshold (* 50 1000 1000))

;; A function to help us reload the config eithout needing to open the init.el file
(defun reload-user-init-file()
  "A function to reload the Emacs config without needing to open the init.el file"
  (interactive)
  (load-file user-init-file))

;; Profile
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Package management
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Sane defaults
(setq-default
 use-package-always-ensure t		; Always ensure
 kill-ring-max 300                      ; Set kill-ring to max 300
 indent-tabs-mode nil                   ; Use spaces instead of tabs
 tab-width 2                            ; Set tab width to 2 spaces
 fill-column 80                         ; Set fill column to 80 characters
 backup-by-copying t                    ; Don't create backup files
 version-control t                      ; use versioned backups
 delete-old-versions t
 backup-directory-alist '((expand-file-name "backup" user-emacs-directory))
 kept-new-versions 6
 kept-old-versions 2
 auto-save-default nil                  ; Don't auto-save
 column-number-mode t                   ; Show column number in mode line
 inhibit-startup-screen t               ; Disable startup screen
 initial-scratch-message nil            ; Empty *scratch* buffer
 ring-bell-function 'ignore             ; Disable bell
 )

(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep buffers automatically up to date
(global-auto-revert-mode t)

;; context menu
(setq context-menu-functions
      '(context-menu-ffap
        occur-context-menu
        context-menu-region
        context-menu-undo
        context-menu-dictionary))

;; load all .el in lisp directory
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (load-file file))))

;; Load custom settings from custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; start server
;;(server-start)

(provide 'init)

