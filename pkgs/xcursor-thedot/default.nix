{ lib, stdenvNoCC, fetchzip }:

stdenvNoCC.mkDerivation rec {
  pname = "xcursor-thedot";
  version = "0.6";

  # Stable source from Bitbucket
  src = fetchzip {
    url = "https://bitbucket.org/sergiy_ilchuk/thedot/get/0362c13478d3.zip";
    sha256 = "0jzm4z8qadh9p2pkg3szamdp2izn72grr49mklbp97xqd4z10223";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/icons

    # The archive extracts to a directory with the commit hash
    # Find and copy cursor directories
    for variant in "TheDOT 0.3" "TheDOT 0.3 transparent" "TheDOT 0.3 light"; do
      if [ -d "$variant" ]; then
        case "$variant" in
          "TheDOT 0.3")
            cp -r "$variant" "$out/share/icons/Dot-Dark"
            # Update theme name in index.theme
            sed -i 's/TheDOT 0.3/Dot-Dark/g' "$out/share/icons/Dot-Dark/index.theme" 2>/dev/null || true
            ;;
          "TheDOT 0.3 transparent")
            cp -r "$variant" "$out/share/icons/Dot-Transparent"
            sed -i 's/TheDOT 0.3 transparent/Dot-Transparent/g' "$out/share/icons/Dot-Transparent/index.theme" 2>/dev/null || true
            ;;
          "TheDOT 0.3 light")
            cp -r "$variant" "$out/share/icons/Dot-Light"
            sed -i 's/TheDOT 0.3 light/Dot-Light/g' "$out/share/icons/Dot-Light/index.theme" 2>/dev/null || true
            ;;
        esac
      fi
    done

    # Create symlinks for missing cursor types (GNOME Shell compatibility)
    for theme in $out/share/icons/Dot-*; do
      if [ -d "$theme/cursors" ]; then
        cd "$theme/cursors"
        # DnD cursors
        [ ! -e dnd-ask ] && [ -e dnd-none ] && ln -sf dnd-none dnd-ask
        [ ! -e dnd-copy ] && [ -e dnd-none ] && ln -sf dnd-none dnd-copy
        [ ! -e dnd-link ] && [ -e dnd-none ] && ln -sf dnd-none dnd-link
        [ ! -e dnd-move ] && [ -e dnd-none ] && ln -sf dnd-none dnd-move
      fi
    done

    runHook postInstall
  '';

  meta = with lib; {
    description = "TheDot cursor theme - circular dot-based cursor designs";
    homepage = "https://store.kde.org/p/1244392";
    license = licenses.gpl3;
    platforms = platforms.linux;
  };
}
