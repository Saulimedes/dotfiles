{
  pkgs,
  pkgs-unstable,
  ...
}:
let
  emacsPackage = (pkgs-unstable.emacsPackagesFor pkgs-unstable.emacs-pgtk).emacsWithPackages (epkgs: [
    epkgs.vterm
    epkgs.pdf-tools
    epkgs.treesit-grammars.with-all-grammars
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
      SHELL = "/run/current-system/sw/bin/zsh";
      COLORTERM = "truecolor";
    };
  };
}
