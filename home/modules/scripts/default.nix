# Custom scripts
{ config, pkgs, lib, ... }:

let
  # Helper to create script packages
  mkScript = name: text: pkgs.writeShellScriptBin name text;

  # Extract archive script
  extractScript = mkScript "extract" ''
    for arg in "$@"; do
      if [ -f "''${arg}" ] ; then
        case "''${arg}" in
          *.tar.bz2)   tar -xvf "''${arg}"     ;;
          *.tar.gz)    tar -xvzf "''${arg}"    ;;
          *.tar.xz|*.txz) tar -xvJf "''${arg}"  ;;
          *.lzma)      ${pkgs.xz}/bin/unlzma "''${arg}"        ;;
          *.bz2)       ${pkgs.bzip2}/bin/bunzip2 "''${arg}"       ;;
          *.rar)       ${pkgs.unrar}/bin/unrar x -ad "''${arg}"   ;;
          *.gz)        ${pkgs.gzip}/bin/gunzip "''${arg}"        ;;
          *.tar)       tar -xvf "''${arg}"      ;;
          *.tbz2)      tar -xvjf "''${arg}"     ;;
          *.tgz)       tar -xvzf "''${arg}"     ;;
          *.zip)       ${pkgs.unzip}/bin/unzip "''${arg}"         ;;
          *.Z)         ${pkgs.ncompress}/bin/uncompress "''${arg}"    ;;
          *.7z)        ${pkgs.p7zip}/bin/7z x "''${arg}"          ;;
          *.xz)        ${pkgs.xz}/bin/unxz "''${arg}"          ;;
          *.exe)       ${pkgs.cabextract}/bin/cabextract "''${arg}"    ;;
          *)           echo "extract: ''${arg} - unknown archive method" ;;
        esac
      else
        echo "Error: ''${arg} - file does not exist"
        exit 1
      fi
    done
  '';

  # Base64 encoding/decoding
  b64encScript = mkScript "b64enc" ''
    echo -n "$1" | ${pkgs.coreutils}/bin/base64
  '';

  b64decScript = mkScript "b64dec" ''
    echo "$1" | ${pkgs.coreutils}/bin/base64 -d
  '';

  # Clipboard utilities
  copyScript = mkScript "copy" ''
    if [ -n "$WAYLAND_DISPLAY" ]; then
      ${pkgs.wl-clipboard}/bin/wl-copy
    else
      ${pkgs.xclip}/bin/xclip -selection clipboard
    fi
  '';

  pasteScript = mkScript "paste" ''
    if [ -n "$WAYLAND_DISPLAY" ]; then
      ${pkgs.wl-clipboard}/bin/wl-paste
    else
      ${pkgs.xclip}/bin/xclip -selection clipboard -o
    fi
  '';

  # Certificate utilities
  checkcertScript = mkScript "checkcert" ''
    ${pkgs.openssl}/bin/openssl x509 -noout -text -in "$1"
  '';

  checkkeyScript = mkScript "checkkey" ''
    ${pkgs.openssl}/bin/openssl rsa -noout -text -in "$1"
  '';

  checkkeymatchScript = mkScript "checkkeymatch" ''
    cert_md5=$(${pkgs.openssl}/bin/openssl x509 -noout -modulus -in "$1" | ${pkgs.openssl}/bin/openssl md5)
    key_md5=$(${pkgs.openssl}/bin/openssl rsa -noout -modulus -in "$2" | ${pkgs.openssl}/bin/openssl md5)
    echo "Cert: $cert_md5"
    echo "Key:  $key_md5"
    if [ "$cert_md5" = "$key_md5" ]; then
      echo "MATCH"
    else
      echo "NO MATCH"
    fi
  '';

  # Archive creation
  maketargzScript = mkScript "maketargz" ''
    tar -czvf "''${1}.tar.gz" "$1"
  '';

  makezipScript = mkScript "makezip" ''
    ${pkgs.zip}/bin/zip -r "''${1}.zip" "$1"
  '';

  # YouTube download utilities
  ydlScript = mkScript "ydl" ''
    NO_PLAYLIST=0
    VIDEO=""

    for arg in "$@"; do
      case "$arg" in
        n)
          NO_PLAYLIST=1
          ;;
        *)
          VIDEO="$arg"
          ;;
      esac
    done

    [ -z "$VIDEO" ] && VIDEO="$(paste)"

    [ -z "$VIDEO" ] && {
      echo "No URL provided or found in clipboard." >&2
      exit 1
    }

    if [ "$NO_PLAYLIST" -eq 1 ]; then
      ${pkgs.yt-dlp}/bin/yt-dlp --no-playlist "$VIDEO"
    else
      ${pkgs.yt-dlp}/bin/yt-dlp "$VIDEO"
    fi
  '';

  ydaScript = mkScript "yda" ''
    VIDEO="$1"
    [ -z "$VIDEO" ] && VIDEO="$(paste)"
    ${pkgs.yt-dlp}/bin/yt-dlp -x --audio-format mp3 "$VIDEO"
  '';

  ydlwScript = mkScript "ydlw" ''
    VIDEO="$1"
    [ -z "$VIDEO" ] && VIDEO="$(paste)"
    ${pkgs.yt-dlp}/bin/yt-dlp --write-subs --sub-lang en "$VIDEO"
  '';

in
{
  home.packages = [
    extractScript
    b64encScript
    b64decScript
    copyScript
    pasteScript
    checkcertScript
    checkkeyScript
    checkkeymatchScript
    maketargzScript
    makezipScript
    ydlScript
    ydaScript
    ydlwScript
  ];
}
