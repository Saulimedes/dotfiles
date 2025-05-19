;; Disable some UI elements
(setq inhibit-startup-message t)
(setq-default frame-title-format '("%b - Emacs"))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Suppress all game score writing
(setq gamegrid-user-score-file-directory "/dev/null"
      tetris-score-file "/dev/null"
      snake-score-file "/dev/null")

;; Suppress "game dir ... Permission denied" warning from message()
(defun suppress-game-dir-warning (orig-fn format-string &rest args)
  "Suppress Emacs game dir permission warnings in minibuffer."
  (unless (and (stringp format-string)
               (string-match-p "game dir.*Permission denied" format-string))
    (apply orig-fn format-string args)))

(advice-add 'message :around #'suppress-game-dir-warning)
