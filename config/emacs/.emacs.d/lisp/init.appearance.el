;; Set preset Size of the Frame 
(setq default-frame-alist
      '((width . 160)  ; Set the window width to 100 columns
        (height . 60))) ; Set the window height to 40 rows

;; Set the font and enable ligatures for the default face
(set-face-attribute 'default nil
                    :font "VictorMono Nerd Font Propo Medium" ;; 
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
;(use-package nord-theme :defer t)

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
(use-package nord-theme
  ;; (vertical-bar   (doom-darken base5 0.4))
  ;; (doom-darken bg 0.4)
  :config
  (load-theme 'nord t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  ;; Modeline
  (use-package doom-modeline
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon nil)
    (doom-modeline-minor-modes nil)
    :hook
    (after-init . doom-modeline-mode)
    :config
    (set-cursor-color "cyan")
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main
      '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))


;; all the icons
(use-package all-the-icons
  :if (display-graphic-p))

;; page-break-lines-mode
(use-package page-break-lines
  :if (display-graphic-p))

;; Dashboard
(use-package dashboard
  :after all-the-icons page-break-lines
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
