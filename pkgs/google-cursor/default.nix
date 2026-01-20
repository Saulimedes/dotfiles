{ lib, stdenvNoCC, fetchurl }:

stdenvNoCC.mkDerivation rec {
  pname = "google-cursor";
  version = "2.0.0";

  srcs = [
    (fetchurl {
      url = "https://github.com/ful1e5/Google_Cursor/releases/download/v${version}/GoogleDot-Blue.tar.gz";
      sha256 = "17bj5x1f5x3am77g13g0d2gqy4sx39ybxrhziika3m3ikkrsrksm";
    })
    (fetchurl {
      url = "https://github.com/ful1e5/Google_Cursor/releases/download/v${version}/GoogleDot-Black.tar.gz";
      sha256 = "1qq0pnpzqi582f8d6rixiyc594chmz2dp354l4k8dh7smg64vn7r";
    })
    (fetchurl {
      url = "https://github.com/ful1e5/Google_Cursor/releases/download/v${version}/GoogleDot-White.tar.gz";
      sha256 = "1c6dndiqgj4xnhkkpz3fp4yr6qdz148rhwpw07pgv5ymss096dnd";
    })
    (fetchurl {
      url = "https://github.com/ful1e5/Google_Cursor/releases/download/v${version}/GoogleDot-Red.tar.gz";
      sha256 = "03fmxwdhli0xbcpizm7gdk4w928iyqlhwmn68ihrn3pxsa97vcrd";
    })
  ];

  sourceRoot = ".";

  unpackPhase = ''
    for src in $srcs; do
      tar xzf "$src"
    done
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/icons

    for dir in GoogleDot-*; do
      if [ -d "$dir" ]; then
        cp -r "$dir" $out/share/icons/
      fi
    done

    runHook postInstall
  '';

  meta = with lib; {
    description = "Google cursor theme inspired by Google";
    homepage = "https://github.com/ful1e5/Google_Cursor";
    license = licenses.gpl3;
    platforms = platforms.linux;
    maintainers = [ ];
  };
}
