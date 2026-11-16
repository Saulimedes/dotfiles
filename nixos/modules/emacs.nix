{
  pkgs,
  pkgs-unstable,
  ...
}:
let
  emacsPackage = (pkgs-unstable.emacsPackagesFor pkgs-unstable.emacs-pgtk).emacsWithPackages (epkgs: [
    epkgs.treesit-grammars.with-all-grammars
    epkgs.agent-shell
    epkgs.aggressive-indent
    epkgs.ansible
    epkgs.ansible-doc
    epkgs.apache-mode
    epkgs.auctex
    epkgs.benchmark-init
    epkgs.blamer
    epkgs.breadcrumb
    epkgs.browse-at-remote
    epkgs.calibredb
    epkgs.cape
    epkgs.consult
    epkgs.consult-dir
    epkgs.consult-eglot
    epkgs.corfu
    epkgs.csv-mode
    epkgs.diff-hl
    epkgs.docker
    epkgs.dockerfile-mode
    epkgs.dumb-jump
    epkgs.ebuild-mode
    epkgs.editorconfig
    epkgs.eglot
    epkgs.elf-mode
    epkgs.elfeed
    epkgs.elfeed-protocol
    epkgs.embark
    epkgs.embark-consult
    epkgs.emmet-mode
    epkgs.emojify
    epkgs.envrc
    epkgs.esh-autosuggest
    epkgs.eshell-syntax-highlighting
    epkgs.eshell-up
    epkgs.esup
    epkgs.exec-path-from-shell
    epkgs.f
    epkgs.flymake-shellcheck
    epkgs.fontaine
    epkgs.format-all
    epkgs.gcmh
    epkgs.git-messenger
    epkgs.git-timemachine
    epkgs.gptel
    epkgs.gptel-commit
    epkgs.graphql-mode
    epkgs.hcl-mode
    epkgs.helpful
    epkgs.highlight-indent-guides
    epkgs.hl-todo
    epkgs.iedit
    epkgs.imenu-list
    epkgs.jinja2-mode
    epkgs.json-mode
    epkgs.k8s-mode
    epkgs.kubel
    epkgs.kubernetes
    epkgs.logview
    epkgs.magit
    epkgs.marginalia
    epkgs.markdown-mode
    epkgs.meow
    epkgs.modus-themes
    epkgs.move-text
    epkgs.multi-vterm
    epkgs.multiple-cursors
    epkgs.nerd-icons-corfu
    epkgs.nginx-mode
    epkgs.nov
    epkgs.orderless
    epkgs.org-modern
    epkgs.org-pomodoro
    epkgs.org-roam
    epkgs.org-super-agenda
    epkgs.ox-epub
    epkgs.page-break-lines
    epkgs.pass
    epkgs.pcap-mode
    epkgs.pdf-tools
    epkgs.pinentry
    epkgs.projectile
    epkgs.pulsar
    epkgs.rainbow-delimiters
    epkgs.rainbow-mode
    epkgs.restart-emacs
    epkgs.restclient
    epkgs.scss-mode
    epkgs.shell-pop
    epkgs.solaire-mode
    epkgs.spacious-padding
    epkgs.sql-indent
    epkgs.ssh-config-mode
    epkgs.super-save
    epkgs.symbol-overlay
    epkgs.systemd
    epkgs.tldr
    epkgs.toml-mode
    epkgs.typescript-mode
    epkgs.vertico
    epkgs.visual-fill-column
    epkgs.vterm
    epkgs.vundo
    epkgs.wallabag
    epkgs.web-mode
    epkgs.wgrep
    epkgs.which-key
    epkgs.winum
    epkgs.x509-mode
    epkgs.yaml-mode
    epkgs.yasnippet
    epkgs.yasnippet-snippets
    epkgs.zig-mode
  ]);

  # init.code-tools.el expects /usr/bin/epdfinfo (served by envfs from PATH)
  epdfinfo = pkgs.runCommand "epdfinfo-link" { } ''
    mkdir -p $out/bin
    ln -s ${pkgs-unstable.emacsPackages.pdf-tools}/share/emacs/site-lisp/elpa/pdf-tools-*/epdfinfo $out/bin/epdfinfo
  '';
in
{
  environment.systemPackages = [
    emacsPackage
    epdfinfo
  ];

  systemd.user.services.emacs = {
    description = "Emacs daemon";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    # first start bootstraps straight.el and takes minutes
    serviceConfig = {
      Type = "notify";
      NotifyAccess = "all";
      TimeoutStartSec = "infinity";
      ExecStart = "${emacsPackage}/bin/emacs --fg-daemon";
      ExecStop = "${emacsPackage}/bin/emacsclient --eval '(kill-emacs)'";
      Restart = "on-failure";
      RestartSec = 3;
    };
    path = [
      "/run/current-system/sw"
      "/etc/profiles/per-user/becker"
    ];
    environment = {
      SHELL = "/run/current-system/sw/bin/fish";
      COLORTERM = "truecolor";
    };
  };
}
