;; -*- lexical-binding: t; -*-
;; Performance optimizations for Emacs

;; ============================================================
;; Garbage Collection Magic Hack
;; ============================================================

;; Minimize GC during normal use, run during idle
(use-package gcmh
  :demand t
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 32 1024 1024)  ; 32MB
        gcmh-verbose nil)
  (gcmh-mode 1))

;; ============================================================
;; Native Compilation
;; ============================================================

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-deferred-compilation t
        native-comp-async-jobs-number 4
        native-compile-prune-cache t))

;; ============================================================
;; Process & I/O Performance
;; ============================================================

;; Larger read buffer for LSP/external processes
(setq read-process-output-max (* 4 1024 1024))  ; 4MB
(setq process-adaptive-read-buffering nil)

;; Faster file loading
(setq large-file-warning-threshold (* 50 1024 1024))  ; 50MB

;; ============================================================
;; Display Performance
;; ============================================================

;; Font-lock (syntax highlighting)
(setq font-lock-maximum-decoration t
      font-lock-maximum-size (* 1024 1024))  ; 1MB
(setq jit-lock-defer-time 0.05
      jit-lock-stealth-time 0.5
      jit-lock-stealth-nice 0.5)

;; Bidirectional text (disable for LTR-only)
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Long lines handling
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(global-so-long-mode 1)

;; Reduce rendering work
(setq idle-update-delay 1.0
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;; ============================================================
;; Compilation & Warnings
;; ============================================================

(setq byte-compile-warnings '(not obsolete cl-functions))
(setq warning-suppress-log-types '((comp)))
(setq ad-redefinition-action 'accept)

;; ============================================================
;; Startup Profiling (optional)
;; ============================================================

(use-package benchmark-init
  :defer t
  :commands (benchmark-init/activate benchmark-init/show-durations-tree)
  :config
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

(use-package esup
  :defer t
  :commands esup
  :config
  (setq esup-depth 0))

;; ============================================================
;; Startup message
;; ============================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %.2fs with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'init.performance)
