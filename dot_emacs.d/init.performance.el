;; Optional benchmark initialization - only load if available
(condition-case nil
    (use-package benchmark-init
      :ensure t
      :config
      (benchmark-init/activate)
      ;; To disable collection of benchmark data after init is done.
      (add-hook 'after-init-hook 'benchmark-init/deactivate))
  (error (message "benchmark-init package not available")))

;; Optional profiling tool - only load if needed
(condition-case nil
    (use-package esup
      :ensure t
      :commands (esup)
      :init
      (setq esup-depth 0
            esup-user-init-file user-init-file))
  (error (message "esup package not available")))

;; Native compilation settings
(when (fboundp 'native-comp-available-p)
  (when (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors nil ; Silence compiler warnings
          native-comp-deferred-compilation t           ; Enable deferred compilation
          native-comp-async-jobs-number 4)))          ; Parallelize compilation

;; Faster startup by not running package-initialize a second time
(setq package-enable-at-startup nil)

;; Reduce font-lock if necessary in large buffers
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size (* 1024 1024)) ; 1MB before disabling fontification
(global-font-lock-mode t)
(setq jit-lock-defer-time 0.05)
(setq jit-lock-stealth-time 0.5)

;; Disable bidirectional text scanning for performance
(setq-default bidi-display-reordering nil)

;; More efficient file-handling
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
(setq process-adaptive-read-buffering nil)

;; Silence cl package deprecation warning
(setq byte-compile-warnings '(not cl-functions obsolete))
(with-eval-after-load 'package
  (setq package-check-signature nil))

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