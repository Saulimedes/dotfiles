;; Disable some UI elements
(setq inhibit-startup-message t)
(setq-default frame-title-format '("%b - Emacs"))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; CRITICAL: Completely disable game scores to prevent Permission denied warnings
;; This must happen in early-init.el to take effect before the warning occurs
(setq game-directory nil
      games-directory nil
      game-score-directory nil
      tetris-score-file "/dev/null"
      snake-score-file "/dev/null"
      gamegrid-user-score-file-directory nil)

;; Create a fake directory to satisfy game directory checks
(ignore-errors
  (make-directory "~/.emacs.d/games" t)
  (with-temp-file "~/.emacs.d/games/DISABLE_GAMES"
    (insert "This file is here to disable game score warnings.\n")))

;; Forcefully redefine the make-game-score-directory function to prevent warnings
(eval-after-load 'games
  '(progn
     ;; Replace the function that checks game directory permissions
     (defun make-game-score-directory ()
       "Don't do anything, to prevent permission denied warnings.")
     ;; Also redefine the function that gets directory
     (defun game-directory ()
       "Return nil to disable game scoring."
       nil)))

;; Install advice to suppress any remaining game directory warnings
(defun suppress-game-dir-warning (orig-fn format-string &rest args)
  "Filter out game directory permission warnings."
  (unless (and (stringp format-string)
               (string-match-p "game dir.*Permission denied" format-string))
    (apply orig-fn format-string args)))

(advice-add 'message :around #'suppress-game-dir-warning)

