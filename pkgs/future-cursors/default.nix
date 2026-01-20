{ lib, stdenvNoCC, fetchFromGitHub, inkscape, xcursorgen }:

stdenvNoCC.mkDerivation rec {
  pname = "future-cursors";
  version = "2.0.1";

  src = fetchFromGitHub {
    owner = "yeyushengfan258";
    repo = "Future-cursors";
    rev = "587c14d2f5bd2dc34095a4efbb1a729eb72a1d36";
    sha256 = "09hpgc21v3c31fx65w74h5w42pnn9a18s4g5m7rifmhdmcqj08ff";
  };

  nativeBuildInputs = [ inkscape xcursorgen ];

  buildPhase = ''
    runHook preBuild

    # Create output directories
    mkdir -p dist/Future-dark-cursors/cursors
    mkdir -p dist/Future-light-cursors/cursors

    patchShebangs .

    # Build the cursors
    HOME=$TMPDIR ./build.sh

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/icons

    # Install both dark and light variants
    for variant in dist/Future-*-cursors; do
      if [ -d "$variant" ]; then
        cp -r "$variant" $out/share/icons/
      fi
    done

    runHook postInstall
  '';

  meta = with lib; {
    description = "Future cursors - an x-cursor theme inspired by macOS";
    homepage = "https://github.com/yeyushengfan258/Future-cursors";
    license = licenses.gpl3;
    platforms = platforms.linux;
    maintainers = [ ];
  };
}
