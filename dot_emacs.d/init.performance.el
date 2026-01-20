;; -*- lexical-binding: t; -*-
;; Performance optimizations for runtime Emacs

;; ============================================================
;; GCMH - Garbage Collection Magic Hack
;; ============================================================
;; Minimize GC interference during normal use, run GC when idle
(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-idle-delay 5           ; GC after 5 seconds idle
        gcmh-high-cons-threshold (* 64 1024 1024)  ; 64MB during activity
        gcmh-low-cons-threshold (* 16 1024 1024)   ; 16MB when idle
        gcmh-verbose nil)           ; Silent operation
  :config
  (gcmh-mode 1))

;; ============================================================
;; Optional profiling tools (only load when needed)
;; ============================================================
(use-package benchmark-init
  :defer t
  :commands (benchmark-init/activate benchmark-init/show-durations-tree)
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package esup
  :defer t
  :commands (esup)
  :init
  (setq esup-depth 0
        esup-user-init-file user-init-file))

;; ============================================================
;; Native compilation
;; ============================================================
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-async-jobs-number (max 1 (/ (num-processors) 2))
        native-comp-speed 2))  ; Optimize for speed

;; ============================================================
;; Font-lock and rendering performance
;; ============================================================
(setq font-lock-maximum-decoration t
      font-lock-maximum-size (* 2 1024 1024)) ; 2MB before disabling

(setq jit-lock-defer-time 0
      jit-lock-stealth-time 1.0
      jit-lock-stealth-nice 0.5
      jit-lock-chunk-size 4096)

;; Disable bidirectional text for LTR-only use
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; ============================================================
;; Process and I/O performance
;; ============================================================
(setq read-process-output-max (* 4 1024 1024)  ; 4MB for LSP
      process-adaptive-read-buffering nil)

;; ============================================================
;; Reduce UI overhead
;; ============================================================
(setq idle-update-delay 1.0           ; Update UI less frequently when idle
      fast-but-imprecise-scrolling t  ; Faster scrolling
      redisplay-skip-fontification-on-input t)  ; Skip fontification during input

;; Inhibit compacting font caches during GC (improves font rendering perf)
(setq inhibit-compacting-font-caches t)

;; Don't ping random machines (also a security improvement)
(setq ffap-machine-p-known 'reject)

;; Silence cl package deprecation warning
(setq byte-compile-warnings '(not cl-functions obsolete))

;; Prevent game directory permission errors from displaying
(defun my/suppress-game-errors (orig-fun &rest args)
  "Wrapper to silence errors related to game scores."
  (condition-case nil
      (apply orig-fun args)
    (file-error nil)))

;; Apply advice to game functions that might try to access /var/games
(dolist (fun '(tetris-high-scores snake-high-scores))
  (advice-add fun :around #'my/suppress-game-errors))

;; Display Time - show how long file took to load
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init.performance)