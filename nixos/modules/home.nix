{ ... }:
let
  repo = "https://github.com/Saulimedes/dotfiles.git";
  branch = "nixos";
in
{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "hm-backup";

    users.becker =
      { lib, pkgs, ... }:
      {
        home.stateVersion = "26.05";
        home.packages = [ pkgs.chezmoi ];

        home.pointerCursor = {
          package = pkgs.bibata-cursors;
          name = "Bibata-Modern-Classic";
          size = 20;
          gtk.enable = true;
          x11.enable = true;
        };

        gtk = {
          enable = true;
          theme = {
            name = "Tokyonight-Dark";
            package = pkgs.tokyonight-gtk-theme;
          };
          iconTheme = {
            name = "Papirus-Dark";
            package = pkgs.papirus-icon-theme;
          };
        };

        programs.brave = {
          enable  = true;
          package = null; # brave-origin is installed system-wide already; just write the extensions
          extensions = [
            { id = "nngceckbapebfimnlniiiahkandclblb"; } # Bitwarden Password Manager
            { id = "ponfpcnoihfmfllpaingbgckeeldkhle"; } # Enhancer for YouTube
            { id = "cclelndahbckbenkjhflpdbgdldlbecc"; } # Get cookies.txt LOCALLY
            { id = "gebbhagfogifgggkldgodflihgfeippi"; } # Return YouTube Dislike
            { id = "mnjggcdmjocbbbhaepdhchncahnbgone"; } # SponsorBlock for YouTube
            { id = "dhdgffkkebhmkfjojejmpbldmpobfkfo"; } # Tampermonkey
            { id = "gbmgphmejlcoihgedabhgjdkcahacjlj"; } # Wallabagger
          ];
        };

        home.activation.chezmoi = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          export PATH=/run/current-system/sw/bin:/etc/profiles/per-user/becker/bin:${
            lib.makeBinPath (
              with pkgs;
              [
                chezmoi
                git
                openssh
              ]
            )
          }:$PATH
          export GIT_TERMINAL_PROMPT=0
          if [ -d "$HOME/.local/share/chezmoi/.git" ]; then
            chezmoi update || chezmoi apply || chezmoi apply --exclude externals \
              || echo "chezmoi: update failed, dotfiles left as they are"
          elif [ -f "$HOME/.config/chezmoi/chezmoi.toml" ]; then
            chezmoi init --apply --branch ${branch} ${repo} || chezmoi apply --exclude externals \
              || echo "chezmoi: init failed"
          else
            echo "chezmoi: ~/.config/chezmoi/chezmoi.toml (name, email) missing, skipping first init"
          fi
        '';
      };
  };

  systemd.services.home-manager-becker = {
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
  };
}
