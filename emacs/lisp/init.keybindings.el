;; Mappings
(global-set-key (kbd "C-x C-k") 'kill-this-buffer) ; Close the current buffer
(global-set-key (kbd "C-c d") 'duplicate-line) ; Duplicate the current line
(global-set-key (kbd "C-c r") 'revert-buffer) ; Revert the current buffer to the saved file
(global-set-key (kbd "M-o") 'other-window) ; Switch to the next window
(global-set-key (kbd "C-c s") 'swiper) ; Search with 'swiper' (requires the 'swiper' package)
(global-set-key (kbd "C-c c") 'comment-line) ; Toggle comments on the current line or selected region
(global-set-key (kbd "C-+") 'text-scale-increase) ; Increase text size
(global-set-key (kbd "C--") 'text-scale-decrease) ; Decrease text size
(global-set-key (kbd "C-0") 'text-scale-set)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)           ; increase the font scale
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)         ; decrease the font scale
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)

;; faster version of C-p and C-n to navigate quickly 
(global-set-key (kbd "C-S-n") (lambda () (interactive) (next-line 8)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (previous-line 8)))
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer-and-window) 

(global-set-key (kbd "C-c k") 'compile) ; Start a compilation process
(global-set-key (kbd "C-c o") 'occur) ; Show all occurrences of a pattern in the current buffer
(global-set-key (kbd "M-%") 'query-replace-regexp) ; Replace text using regular expressions
(global-set-key (kbd "C-c b") 'browse-kill-ring) ; Browse the kill ring (requires the 'browse-kill-ring' package)
(global-set-key (kbd "C-c y") 'yank-pop) ; Cycle through the kill ring after yanking
(global-set-key (kbd "C-c q") 'join-line) ; Join the current line with the line below it
(global-set-key (kbd "C-x p") 'previous-buffer) ; Switch to the previous buffer
(global-set-key (kbd "C-x n") 'next-buffer) ; Switch to the next buffer
(global-set-key (kbd "C-c e") 'eval-region) ; Evaluate selected region as Lisp code
(global-set-key (kbd "C-c u") 'undo-tree-visualize) ; Visualize undo history (requires 'undo-tree' package)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; ESC Cancell Alll
(global-set-key (kbd "C-S-v") 'yank)
(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)                   ; fold the current section
(global-set-key (kbd "C-S-<iso-lefttab>") 'hs-hide-level)            ; fold the sub sections of the current section
(global-set-key (kbd "<backtab>") 'indent-according-to-mode)
 
;; Define a function for duplicating the current line
(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((col (current-column))
        (text (buffer-substring (line-beginning-position) (line-end-position))))
    (forward-line)
    (insert text "\n")
    (forward-line -1)
    (move-to-column col)))

;; Which-Key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; evil
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'init.keybindings)
