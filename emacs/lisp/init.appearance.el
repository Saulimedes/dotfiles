;; Set preset Size of the Frame 
(setq default-frame-alist
      '((width . 160)  ; Set the window width to 100 columns
        (height . 60))) ; Set the window height to 40 rows

;; Set the font and enable ligatures for the default face
(set-face-attribute 'default nil
                    :font "PragmataProLiga Nerd Font" ;; 
                    :height 120) 

(setq frame-inhibit-implied-resize t)
(setq-default line-spacing 1)
(setq auto-composition-mode nil)

;; Highlight matching parentheses
(show-paren-mode 1) ; Highlight matching parentheses

;; pulsar
(use-package pulsar
  :init
  (setq
   pulsar-pulse t
   pulsar-delay 0.03
   pulsar-iterations 5
   pulsar-face 'pulsar-cyan))

(pulsar-global-mode 1)

;; themes
(use-package spacegray-theme :defer t)
(use-package vs-dark-theme :defer t)
(use-package melancholy-theme :defer t)

;; cursor
(setq-default cursor-type 'bar) 

;; line numbers
(use-package display-line-numbers
:ensure nil
:hook (prog-mode . display-line-numbers-mode))

;; ligatures
(require 'ligature)

;; Enable ligatures in all modes (optional)
(global-ligature-mode t)

;; Use the following ligatures
(ligature-set-ligatures 't '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))

;; mode-line
(use-package doom-modeline
    :config
      (doom-modeline-mode 1)
      (setq doom-modeline-modal t)
      (setq doom-modeline-lsp t)
      (setq doom-modeline-modal-icon t)
      (setq doom-modeline-buffer-encoding t)
      (setq doom-modeline-support-imenu t)
      (setq doom-modeline-icon t)
      (setq doom-modeline-major-mode-icon t)
      (setq doom-modeline-major-mode-color-icon t)
      (setq doom-modeline-buffer-state-icon t)
    :hook
      (after-init . doom-modeline-mode))

;; all the icons
(use-package all-the-icons
  :if (display-graphic-p))

;; page-break-lines-mode
(use-package page-break-lines
  :if (display-graphic-p))

;; Dashboard
(use-package dashboard
  :after all-the-icons page-break-lines
  :diminish (dashboard-mode page-break-lines-mode)
  :config
(setq dashboard-startup-banner
  (if (display-graphic-p)
    (expand-file-name "dashboard/banner.png" user-emacs-directory)
  (expand-file-name "dashboard/banner.txt" user-emacs-directory)))
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-banner-logo-title "")
(setq dashboard-set-init-info nil)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
(setq dashboard-footer-icon "")

(setq dashboard-footer-messages '("Don't try to solve serious matters in the middle of the night."))
(setq dashboard-items '((recents . 5)
(bookmarks . 5)
(projects . 5)
(agenda . 5)
(registers . 5)))
(dashboard-setup-startup-hook))
(add-hook 'dashboard-mode-hook 'page-break-lines-mode)

(defun my/dashboard-select-first-item ()
  "Select the first item in the Emacs dashboard."
  (run-with-timer 0.1 nil (lambda ()
                            (when (get-buffer "*dashboard*")
                              (switch-to-buffer "*dashboard*")
                              (goto-char (point-min))
                              (forward-line 2) ; Adjust this number based on your dashboard layout
                              (widget-forward 1)))))

(add-hook 'dashboard-mode-hook 'my/dashboard-select-first-item)

(provide 'init.appearance)
