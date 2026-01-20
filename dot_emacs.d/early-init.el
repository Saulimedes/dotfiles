;; -*- lexical-binding: t; -*-
;; early-init.el - loaded before init.el, before package/UI initialization

;; ============================================================
;; Startup performance optimizations
;; ============================================================

;; Increase GC threshold during startup (reset after init)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent unwanted runtime compilation for native-comp
(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation nil
        native-comp-async-report-warnings-errors 'silent))

;; Prevent package.el from loading packages before init
(setq package-enable-at-startup nil)

;; Disable file-name-handler during startup
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file-name-handler after startup
;; NOTE: GC is managed by gcmh-mode in init.performance.el
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist)))

;; ============================================================
;; UI - disable before frame creation for faster startup
;; ============================================================

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

;; Disable UI elements before frame creation
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Explicit disable (some terminals need this)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Prevent flash of unstyled content
(setq-default mode-line-format nil)

;; ============================================================
;; Frame settings
;; ============================================================

(setq frame-title-format '("%b - Emacs")
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Set background early to prevent white flash
(push '(background-color . "#000000") default-frame-alist)
(push '(foreground-color . "#ffffff") default-frame-alist)

;; ============================================================
;; Misc suppressions
;; ============================================================

;; Suppress game score warnings
(setq gamegrid-user-score-file-directory "/dev/null"
      tetris-score-file "/dev/null"
      snake-score-file "/dev/null")

(defun my/suppress-game-dir-warning (orig-fn format-string &rest args)
  "Suppress Emacs game dir permission warnings."
  (unless (and (stringp format-string)
               (string-match-p "game dir.*Permission denied" format-string))
    (apply orig-fn format-string args)))
(advice-add 'message :around #'my/suppress-game-dir-warning)

;; Silence native-comp warnings
(setq native-comp-async-report-warnings-errors 'silent)
(setq byte-compile-warnings '(not obsolete))

;; ============================================================
;; Paths - keep ~/.emacs.d clean
;; ============================================================

(defvar emacs-cache-directory (expand-file-name "~/.cache/emacs/")
  "Directory for Emacs cache files.")
(make-directory emacs-cache-directory t)

(setq user-emacs-directory emacs-cache-directory)

;; Local variables:
;; no-byte-compile: t
;; End:
