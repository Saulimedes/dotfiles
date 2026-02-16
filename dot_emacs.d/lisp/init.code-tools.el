;; -*- lexical-binding: t; -*-
;; Advanced code navigation and editing tools

;; Jump to definition across files
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

;; Code folding (built-in hideshow, no daemon issues)
(use-package hideshow
  :ensure nil
  :straight nil
  :hook (prog-mode . hs-minor-mode))

;; Quick edit similar items
(use-package iedit
  :bind ("C-c ;" . iedit-mode))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-c C-SPC" . mc/edit-lines)))

;; Code structure outline with imenu-list
(use-package imenu-list
  :bind ("C-'" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

;; Automatic code formatting
(use-package format-all
  :hook (prog-mode . format-all-mode)
  :custom
  (format-all-formatters
   '(("Python" black)
     ("JavaScript" prettier)
     ("TypeScript" prettier)
     ("CSS" prettier)
     ("Rust" rustfmt)
     ("Go" gofmt)
     ("Markdown" prettier)
     ("YAML" prettier))))

;; Rainbow delimiters for better code readability
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Editorconfig support
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Better commented code highlighting
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF4500")
          ("DEBUG"  . "#A020F0")
          ("NOTE"   . "#1E90FF")
          ("HACK"   . "#FFA500")
          ("BUG"    . "#FF0000")
          ("XXX"    . "#1E90FF"))))

;; Move text easily
(use-package move-text
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down)))

;; Keybindings for code tools (Meow compatible)
(global-set-key (kbd "C-c z f") 'hs-hide-block)
(global-set-key (kbd "C-c z o") 'hs-show-block)
(global-set-key (kbd "C-c z r") 'hs-show-all)
(global-set-key (kbd "C-c z m") 'hs-hide-all)
(global-set-key (kbd "C-c z t") 'hs-toggle-hiding)
(global-set-key (kbd "C-c c d") 'xref-find-definitions)
(global-set-key (kbd "C-c c r") 'xref-find-references)
(global-set-key (kbd "C-c c i") 'imenu-list-smart-toggle)
(global-set-key (kbd "C-c c e") 'iedit-mode)
(global-set-key (kbd "C-c c ;") 'iedit-mode)
(global-set-key (kbd "C-c c f") 'format-all-buffer)
(global-set-key (kbd "C-c x e") 'eval-last-sexp)
(global-set-key (kbd "C-c x b") 'eval-buffer)
(global-set-key (kbd "C-c x f") 'eval-defun)

;; ============================================================
;; Markdown
;; ============================================================
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "multimarkdown")
  (markdown-header-scaling t))

;; ============================================================
;; Zig
;; ============================================================
(use-package zig-mode
  :mode "\\.zig\\'"
  :hook (zig-mode . eglot-ensure)
  :custom
  (zig-format-on-save t))

;; ============================================================
;; LaTeX
;; ============================================================
(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-PDF-mode t)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(LaTeX-mode . ("texlab")))))

;; ============================================================
;; Ansible
;; ============================================================
(use-package ansible
  :hook (yaml-mode . (lambda ()
                       (when (or (string-match-p "/\\(roles\\|tasks\\|handlers\\|playbooks\\|group_vars\\|host_vars\\)/" (or buffer-file-name ""))
                                 (string-match-p "\\(playbook\\|site\\|main\\|ansible\\)\\.ya?ml\\'" (or buffer-file-name "")))
                         (ansible 1)))))

(use-package ansible-doc
  :after ansible
  :hook (ansible-hook . ansible-doc-mode))

;; ============================================================
;; Dockerfile
;; ============================================================
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" "\\.dockerfile\\'"))

;; ============================================================
;; Gentoo ebuild
;; ============================================================
(use-package ebuild-mode
  :mode ("\\.ebuild\\'" "\\.eclass\\'"))

;; ============================================================
;; TOML
;; ============================================================
(use-package toml-mode
  :mode ("\\.toml\\'" "Cargo\\.lock\\'"))

;; ============================================================
;; Jinja2
;; ============================================================
(use-package jinja2-mode
  :mode ("\\.j2\\'" "\\.jinja2?\\'"))

;; ============================================================
;; Systemd unit files
;; ============================================================
(use-package systemd
  :mode ("\\.service\\'" "\\.timer\\'" "\\.socket\\'"
         "\\.target\\'" "\\.mount\\'" "\\.path\\'"
         "\\.slice\\'" "\\.scope\\'"))

;; ============================================================
;; Nginx
;; ============================================================
(use-package nginx-mode
  :mode ("nginx\\.conf\\'" "/nginx/.+\\.conf\\'" "/sites-\\(available\\|enabled\\)/"))

;; ============================================================
;; Apache
;; ============================================================
(use-package apache-mode
  :mode ("\\.htaccess\\'" "httpd\\.conf\\'" "apache2?\\.conf\\'" "/sites-\\(available\\|enabled\\)/.*\\.conf\\'"))

;; ============================================================
;; SQL
;; ============================================================
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; ============================================================
;; GraphQL
;; ============================================================
(use-package graphql-mode
  :mode ("\\.graphql\\'" "\\.gql\\'"))

;; ============================================================
;; CSV
;; ============================================================
(use-package csv-mode
  :mode ("\\.csv\\'" "\\.tsv\\'")
  :custom
  (csv-separators '("," ";" "\t")))

;; ============================================================
;; SSH config
;; ============================================================
(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys\\'" . ssh-authorized-keys-mode)))

;; ============================================================
;; Log files
;; ============================================================
(use-package logview
  :mode ("\\.log\\'" "\\.log\\.[0-9]+\\'")
  :custom
  (logview-additional-timestamp-formats
   '(("ISO 8601" (java-pattern . "yyyy-MM-dd HH:mm:ss.SSS")
      (aliases "ISO")))))

;; ============================================================
;; Makefile (enhance built-in)
;; ============================================================
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode t)))

;; ============================================================
;; REST client - HTTP request builder
;; ============================================================
(use-package restclient
  :mode ("\\.http\\'" "\\.rest\\'")
  :bind (:map restclient-mode-map
              ("C-c C-c" . restclient-http-send-current)
              ("C-c C-r" . restclient-http-send-current-raw)
              ("C-c C-n" . restclient-jump-next)
              ("C-c C-p" . restclient-jump-prev)))

;; ============================================================
;; pass - password-store frontend
;; ============================================================
(use-package pass
  :commands (pass)
  :bind ("C-c P" . pass))

(use-package auth-source-pass
  :ensure nil
  :config
  (auth-source-pass-enable))

;; ============================================================
;; x509-mode - TLS certificate inspector
;; ============================================================
(use-package x509-mode
  :mode ("\\.pem\\'" "\\.crt\\'" "\\.cer\\'" "\\.der\\'"))

;; ============================================================
;; ELF binary inspector
;; ============================================================
(use-package elf-mode
  :magic ("\\^?ELF" . elf-mode))

;; ============================================================
;; Flymake-shellcheck - security linting for shell scripts
;; ============================================================
(use-package flymake-shellcheck
  :hook (sh-mode . flymake-shellcheck-load))

;; ============================================================
;; Scan mode - nmap/nikto/testssl output highlighting
;; ============================================================
(use-package scan-mode
  :straight (:host github :repo "PeterMosmans/scan-mode")
  :mode ("\\.nmap\\'" "\\.nikto\\'" "\\.testssl\\'"))

;; ============================================================
;; PCAP - packet capture viewing
;; ============================================================
(use-package pcap-mode
  :mode ("\\.pcap\\'" "\\.pcapng\\'"))

;; ============================================================
;; EasyPG - GPG encryption (built-in)
;; ============================================================
(use-package epa
  :ensure nil
  :config
  (setq epa-pinentry-mode 'loopback
        epa-file-encrypt-to user-mail-address)
  (epa-file-enable))

(setq epg-gpg-program "gpg2")

;; ============================================================
;; Hexl - hex editor (built-in)
;; ============================================================
(use-package hexl
  :ensure nil
  :bind (("C-c X" . hexl-find-file))
  :config
  (setq hexl-bits 8))

;; ============================================================
;; nov.el - EPUB reader
;; ============================================================
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  ;; Remove doc-view epub association so nov-mode wins
  (setq auto-mode-alist
        (cl-remove-if (lambda (entry)
                        (and (string-match-p "epub" (car entry))
                             (not (eq (cdr entry) 'nov-mode))))
                      auto-mode-alist))
  :custom
  (nov-text-width t)
  (nov-variable-pitch nil)
  (nov-header-line-format nil)
  :config
  (defun my/nov-setup ()
    "Configure nov-mode for comfortable reading."
    (display-line-numbers-mode -1)
    (setq-local line-spacing 0.15)
    (visual-line-mode 1)
    (visual-fill-column-mode -1)
    (face-remap-add-relative 'default :family "Fast_Sans" :height 130)
    (face-remap-add-relative 'variable-pitch :family "Fast_Sans" :height 130)
    (face-remap-add-relative 'shr-text :family "Fast_Sans" :height 130)
    (face-remap-add-relative 'fixed-pitch :family "Fast_Mono" :height 120)
    ;; Enable HarfBuzz shaping for calt (Fast Font bold initial letters)
    (let ((table (make-char-table nil)))
      (set-char-table-parent table composition-function-table)
      (dolist (ch (append (number-sequence ?a ?z) (number-sequence ?A ?Z) (list ?\s)))
        (set-char-table-range table ch
          (list (vector ".+" 0 #'font-shape-gstring))))
      (setq-local composition-function-table table)))

  (add-hook 'nov-mode-hook #'my/nov-setup))

;; ============================================================
;; pdf-tools - PDF viewer (replaces DocView)
;; ============================================================
(use-package pdf-tools
  :ensure nil
  :straight nil
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-info-epdfinfo-program "/usr/bin/epdfinfo")
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  :config
  (pdf-tools-install :no-query)
  ;; Midnight mode for dark reading - auto-enable
  (setq pdf-view-midnight-colors '("#c5c8c6" . "#000000"))
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
  ;; Keybindings
  (define-key pdf-view-mode-map (kbd "d") #'pdf-view-midnight-minor-mode)
  (define-key pdf-view-mode-map (kbd "j") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") #'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h") #'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l") #'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "J") #'pdf-view-next-page)
  (define-key pdf-view-mode-map (kbd "K") #'pdf-view-previous-page)
  (define-key pdf-view-mode-map (kbd "g") #'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "G") #'pdf-view-last-page))

;; ============================================================
;; calibredb - Calibre library management
;; ============================================================
(use-package calibredb
  :commands (calibredb calibredb-list calibredb-find-counsel)
  :bind ("C-c B" . calibredb)
  :custom
  (calibredb-root-dir "~/Calibre Library")
  (calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (calibredb-library-alist '(("~/Calibre Library")))
  (calibredb-format-all-the-icons nil)
  (calibredb-size-show t)
  (calibredb-date-width 10))

;; ============================================================
;; Elfeed - RSS/Atom feed reader
;; ============================================================
(use-package elfeed
  :bind (("C-c R" . elfeed))
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" emacs-cache-directory))
  (elfeed-search-filter "@2-weeks-ago +unread")
  (elfeed-search-title-max-width 80)
  (elfeed-search-trailing-width 30)
  :config
  (setq elfeed-feeds
        '(;; Emacs blogs
          ("https://sachachua.com/blog/category/emacs-news/feed" emacs news)
          ("https://protesilaos.com/codelog.xml" emacs prot)
          ("https://www.masteringemacs.org/feed" emacs mastering)
          ("https://irreal.org/blog/?feed=rss2" emacs irreal)
          ("https://karthinks.com/index.xml" emacs karthink)
          ("https://systemcrafters.net/rss/news.xml" emacs systemcrafters)
          ("https://planet.emacslife.com/rss.xml" emacs planet)))

  ;; Faces for tags
  (defface elfeed-search-emacs-tag '((t :foreground "#81a2be"))
    "Face for emacs-tagged entries.")

  ;; Better entry display
  (defun my/elfeed-setup ()
    "Configure elfeed for comfortable reading."
    (visual-line-mode 1)
    (setq-local shr-width 80))
  (add-hook 'elfeed-show-mode-hook #'my/elfeed-setup))

;; ============================================================
;; Emoji & Unicode
;; ============================================================

;; emojify - display and insert emojis
(use-package emojify
  :hook (after-init . global-emojify-mode)
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode github))
  :bind
  (("C-c e e" . emojify-insert-emoji)
   ("C-c e d" . emojify-describe-emoji-at-point)
   ("C-c e l" . emojify-list-emojis)))

;; Built-in Emacs 29+ emoji search (Unicode only, no package needed)
(when (fboundp 'emoji-search)
  (global-set-key (kbd "C-c e s") #'emoji-search))

;; Quick Unicode character insert (built-in)
(global-set-key (kbd "C-c e u") #'insert-char)

(provide 'init.code-tools)