;; -*- lexical-binding: t; -*-

(when (and (file-exists-p "/etc/NIXOS")
           (boundp 'straight-built-in-pseudo-packages))
  (dolist (package '(vterm pdf-tools))
    (add-to-list 'straight-built-in-pseudo-packages package)))

(provide 'init.0-nixos)
