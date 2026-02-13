;; -*- lexical-binding: t; -*-
;; Main init file — GC is managed by gcmh in init.performance.el

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

;; Bootstrap straight.el with comprehensive error handling
(defvar bootstrap-version)
(condition-case err
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage)

      ;; Configure straight.el settings immediately after bootstrap
      (when (featurep 'straight)
        (setq straight-use-package-by-default t
              straight-check-for-modifications '(check-on-save find-when-checking)
              straight-repository-branch "develop"
              straight-recipes-gnu-elpa-use-mirror nil
              straight-recipes-emacsmirror-use-mirror nil)

        ;; Install use-package safely
        (condition-case use-pkg-err
            (straight-use-package 'use-package)
          (error (message "Failed to install use-package via straight.el: %s"
                         (error-message-string use-pkg-err))))))
  (error
   (message "Failed to bootstrap straight.el: %s" (error-message-string err))
   (message "Falling back to built-in package.el")
   ;; Set up basic use-package without straight
   (require 'package)
   (package-initialize)
   (unless (package-installed-p 'use-package)
     (package-refresh-contents)
     (package-install 'use-package))
   (require 'use-package)
   (setq use-package-always-ensure t)))

;; Native compilation cache
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name "eln-cache/" emacs-cache-directory)))

;; Transient settings
(setq transient-history-file (expand-file-name "transient/history.el" emacs-cache-directory))
(setq transient-levels-file (expand-file-name "transient/levels.el" emacs-cache-directory))
(setq transient-values-file (expand-file-name "transient/values.el" emacs-cache-directory))

;; Sane defaults
(setq-default
 display-line-numbers 'relative
 indent-tabs-mode nil
 vc-follow-symlinks t
 use-package-always-ensure t
 kill-ring-max 300
 tab-width 4
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

;; Enable useful minor modes
(electric-pair-mode 1)
(savehist-mode 1)

;; Clipboard integration
(setq select-enable-clipboard t
      select-enable-primary t)

;; Misc better defaults
(setopt use-short-answers t)
(global-auto-revert-mode t)
(setq context-menu-functions
      '(context-menu-ffap
        occur-context-menu
        context-menu-region
        context-menu-undo
        context-menu-dictionary))

;; Tree-sitter support (built-in for Emacs 29+)
(when (fboundp 'treesit-available-p)
  (when (treesit-available-p)
    (setq major-mode-remap-alist
          '((c-mode . c-ts-mode)
            (c++-mode . c++-ts-mode)
            (css-mode . css-ts-mode)
            (js-mode . js-ts-mode)
            (java-mode . java-ts-mode)
            (json-mode . json-ts-mode)
            (python-mode . python-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (rust-mode . rust-ts-mode)))
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))))

;; Better text editing defaults
(add-hook 'text-mode-hook 'visual-line-mode)

;; Loading additional Emacs Lisp files with error handling
(let ((lisp-dir (expand-file-name "lisp" "~/.emacs.d/")))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (condition-case err
          (load-file file)
        (error (message "Error loading %s: %s" file (error-message-string err)))))))

(provide 'init)
