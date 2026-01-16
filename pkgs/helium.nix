# Helium Browser
{ lib
, unzip
, autoPatchelfHook
, stdenv
, fetchurl
, xorg
, libgbm
, cairo
, libudev-zero
, libxkbcommon
, nspr
, nss
, libcupsfilters
, pango
, qt5
, alsa-lib
, atk
, at-spi2-core
, at-spi2-atk
}:

stdenv.mkDerivation rec {
  pname = "helium";
  version = "0.7.10.1";

  src = fetchurl {
    url = "https://github.com/imputnet/helium-linux/releases/download/${version}/helium-${version}-x86_64_linux.tar.xz";
    sha256 = "";  # Will be filled after first build attempt
  };

  nativeBuildInputs = [
    unzip
    autoPatchelfHook
  ];

  autoPatchelfIgnoreMissingDeps = [
    "libQt6Core.so.6"
    "libQt6Gui.so.6"
    "libQt6Widgets.so.6"
  ];

  runtimeDependencies = [ ];

  buildInputs = [
    unzip
    xorg.libxcb
    xorg.libX11
    xorg.libXcomposite
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXrandr
    libgbm
    cairo
    pango
    libudev-zero
    libxkbcommon
    nspr
    nss
    libcupsfilters
    alsa-lib
    atk
    at-spi2-core
    at-spi2-atk
    qt5.qtbase
    qt5.qttools
    qt5.qtx11extras
    qt5.wrapQtAppsHook
  ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    mv * $out/bin/
    mv $out/bin/chrome $out/bin/${pname}
    mkdir -p $out/share/applications

    cat <<INI> $out/share/applications/${pname}.desktop
[Desktop Entry]
Name=Helium
GenericName=Web Browser
Terminal=false
Icon=$out/bin/product_logo_256.png
Exec=$out/bin/${pname}
Type=Application
Categories=Network;WebBrowser;
INI

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/imputnet/helium-linux";
    description = "Helium Browser - A lightweight Chromium-based browser";
    platforms = platforms.linux;
    license = licenses.bsd3;
  };
}
