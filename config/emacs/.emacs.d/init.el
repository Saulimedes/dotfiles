;; Performance
(setq gc-cons-threshold (* 50 1000 1000))

;; Cache directory
(defvar emacs-cache-directory (expand-file-name "~/.cache/emacs/"))
(unless (file-exists-p emacs-cache-directory)
  (make-directory emacs-cache-directory t))

;; Set the GNUPG home directory for package signatures
(setq package-gnupghome-dir (expand-file-name "~/.gnupg"))
(unless (file-exists-p package-gnupghome-dir)
  (make-directory package-gnupghome-dir t))

;; Custom settings
(setq custom-file (expand-file-name "custom.el" emacs-cache-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

;; Package management
(require 'package)
(setq package-user-dir (expand-file-name "elpa" emacs-cache-directory))

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

;; Native compilation cache
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name "eln-cache/" emacs-cache-directory)))

;; Recent files
(setq recentf-save-file (expand-file-name "recentf" emacs-cache-directory))

;; Transient settings
(setq transient-history-file (expand-file-name "transient/history.el" emacs-cache-directory))
(setq transient-levels-file (expand-file-name "transient/levels.el" emacs-cache-directory))
(setq transient-values-file (expand-file-name "transient/values.el" emacs-cache-directory))

;; Sane defaults and other settings
(setq-default
 electric-pair-mode 1
 display-line-numbers 'relative
 indent-tabs-mode nil
 vc-follow-symlinks t
 use-package-always-ensure t
 kill-ring-max 300
 tab-width 2
 fill-column 80
 backup-by-copying t
 version-control t
 delete-old-versions t
 backup-directory-alist `(("." . ,(expand-file-name "backup" emacs-cache-directory)))
 kept-new-versions 6
 kept-old-versions 2
 auto-save-default nil
 column-number-mode t
 inhibit-startup-screen t
 initial-scratch-message nil
 ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(setq context-menu-functions
      '(context-menu-ffap
        occur-context-menu
        context-menu-region
        context-menu-undo
        context-menu-dictionary))

;; Loading additional Emacs Lisp files
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (load-file file))))

(provide 'init)

