;; Performance optimizations for startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset GC after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1000 1000)
                  gc-cons-percentage 0.1)))

;; Run garbage collection when idle
(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

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

;; Bootstrap straight.el
(defvar bootstrap-version)
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
  (load bootstrap-file nil 'nomessage))

;; Configure use-package to use straight.el by default
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Also integrate with use-package
(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-check-for-modifications '(check-on-save find-when-checking))
  (straight-repository-branch "develop") ;; Use the develop branch for latest recipes
  :config
  ;; Refresh straight.el recipe repositories
  (straight-pull-recipe-repositories))

;; Configure package.el for compatibility
(require 'package)
(setq package-user-dir (expand-file-name "elpa" emacs-cache-directory))
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; Native compilation cache
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name "eln-cache/" emacs-cache-directory)))

;; Recent files with better defaults
(use-package recentf
  :ensure nil
  :init
  (setq recentf-save-file (expand-file-name "recentf" emacs-cache-directory)
        recentf-max-saved-items 200
        recentf-max-menu-items 15)
  :config
  (recentf-mode))

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
 ;; Disable all game score functionality
 gamegrid-user-score-file-directory nil
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

;; Misc better defaults
(defalias 'yes-or-no-p 'y-or-n-p)
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
    ;; Built-in treesit is available
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
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Games are completely disabled in early-init.el

;; Performance optimization
(load-file (expand-file-name "init.performance.el" user-emacs-directory))

;; Loading additional Emacs Lisp files
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (load-file file))))

(provide 'init)

