;; -*- lexical-binding: t; -*-

;; Nix-provided packages sit on load-path but, unlike straight.el, nothing
;; loads their *-autoloads.el, so :init-time calls to autoloaded functions
;; (e.g. vertico-mode) fail with "void function" until something else
;; requires the package first. Load every autoloads file on load-path.
(when (file-exists-p "/etc/NIXOS")
  (dolist (dir load-path)
    (dolist (autoloads (file-expand-wildcards (expand-file-name "*-autoloads.el" dir) t))
      (load autoloads nil 'nomessage))))

(when (and (file-exists-p "/etc/NIXOS")
           (boundp 'straight-built-in-pseudo-packages))
  (dolist (package '(agent-shell aggressive-indent ansible ansible-doc apache-mode
                      auctex benchmark-init blamer breadcrumb browse-at-remote
                      calibredb cape consult consult-dir consult-eglot corfu
                      csv-mode diff-hl docker dockerfile-mode dumb-jump ebuild-mode
                      editorconfig eglot elf-mode elfeed elfeed-protocol embark
                      embark-consult emmet-mode emojify envrc esh-autosuggest
                      eshell-syntax-highlighting eshell-up esup exec-path-from-shell
                      f flymake-shellcheck fontaine format-all gcmh git-messenger
                      git-timemachine gptel gptel-commit graphql-mode hcl-mode
                      helpful highlight-indent-guides hl-todo iedit imenu-list
                      jinja2-mode json-mode k8s-mode kubel kubernetes logview
                      magit marginalia markdown-mode meow modus-themes move-text
                      multi-vterm multiple-cursors nerd-icons-corfu nginx-mode nov
                      orderless org-modern org-pomodoro org-roam org-super-agenda
                      ox-epub page-break-lines pass pcap-mode pdf-tools pinentry
                      projectile pulsar rainbow-delimiters rainbow-mode
                      restart-emacs restclient scss-mode shell-pop solaire-mode
                      spacious-padding sql-indent ssh-config-mode super-save
                      symbol-overlay systemd tldr toml-mode typescript-mode
                      vertico visual-fill-column vterm vundo wallabag web-mode
                      wgrep which-key winum x509-mode yaml-mode yasnippet
                      yasnippet-snippets zig-mode))
    (add-to-list 'straight-built-in-pseudo-packages package)))

(provide 'init.0-nixos)
