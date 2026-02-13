;; -*- lexical-binding: t; -*-
;; TRAMP - Remote access with multi-hop support

(use-package tramp
  :ensure nil
  :config
  (setq tramp-default-method "ssh"
        tramp-verbose 1
        tramp-auto-save-directory (expand-file-name "tramp-autosave" emacs-cache-directory)
        ;; Performance: cache remote file attributes
        remote-file-name-inhibit-cache nil
        tramp-completion-reread-directory-timeout nil
        ;; Use ssh ControlMaster for speed
        tramp-use-ssh-controlmaster-options t
        tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto "
                "-o ControlPath=/tmp/ssh-tramp-%%r@%%h:%%p "
                "-o ControlPersist=1h"))

  ;; Multi-hop: access hosts through jump boxes
  ;; Usage: /ssh:jumpbox|ssh:internal-host:/path/to/file
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))

  ;; sudo on remote: /ssh:host|sudo:host:/etc/config
  (add-to-list 'tramp-default-proxies-alist
               '((regexp-quote (system-name)) nil nil)))

;; Helper: open file on remote as root
(defun my/tramp-sudo-remote (host file)
  "Open FILE on HOST via sudo through SSH."
  (interactive "sHost: \nsFile: ")
  (find-file (format "/ssh:%s|sudo:%s:%s" host host file)))

;; Helper: open remote shell
(defun my/tramp-shell (host)
  "Open eshell on remote HOST."
  (interactive "sHost: ")
  (let ((default-directory (format "/ssh:%s:" host)))
    (eshell t)))

(provide 'init.tramp)
