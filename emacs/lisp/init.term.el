;; Inherits environment variables from the shell
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; fish
(use-package fish-mode
  :mode "\\.fish\\'")

;; shell-pop
(use-package shell-pop
  :bind
  ("C-c t t" . shell-pop) ; Set the keybinding for shell-pop (modify as needed)
  :custom
  (shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
  (shell-pop-window-size 30) ; Set the window size (in percentage)
  (shell-pop-full-span t) ; Make the shell span the full width/height of the Emacs frame
  (shell-pop-window-position "bottom") ; Set the shell window position
  (shell-pop-restore-window-configuration t) ; Restore the window configuration after closing the shell
  (shell-pop-autocd-to-working-dir t)) ; Automatically change the shell directory to the current buffer's

;; Set the default shell for ansi-term
(setq term-default-shell "/usr/bin/fish") ; Replace with your desired shell path

;; Functions to run ansi-term in horizontal or vertical split
(defun split-horizontal-and-run-ansi-term ()
  "Split the window horizontally and run ansi-term with the specified shell."
  (interactive)
  (split-window-below)
  (other-window 1)
  (ansi-term term-default-shell))

(defun split-vertical-and-run-ansi-term ()
  "Split the window vertically and run ansi-term with the specified shell."
  (interactive)
  (split-window-right)
  (other-window 1)
  (ansi-term term-default-shell))

;; Keybindings to open ansi-term in horizontal or vertical split
(global-set-key (kbd "C-c t h") 'split-horizontal-and-run-ansi-term)
(global-set-key (kbd "C-c t v") 'split-vertical-and-run-ansi-term)

;; mapping
(defun my-term-send-ctrl-backspace ()
  "Send the key sequence for Ctrl+Backspace to the terminal."
  (interactive)
  (term-send-raw-string "\b"))

(defun my-term-send-ctrl-delete ()
  "Send the key sequence for Ctrl+Delete to the terminal."
  (interactive)
  (term-send-raw-string "\e\[3\;5~"))

(with-eval-after-load 'term
  (define-key term-raw-map (kbd "C-<backspace>") 'my-term-send-ctrl-backspace)
  (define-key term-raw-map (kbd "C-<delete>") 'my-term-send-ctrl-delete))

(provide 'init.term)
