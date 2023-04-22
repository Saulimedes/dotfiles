#!/bin/sh

sudo zypper refresh
sudo zypper -n update
sudo zypper -n install curl \
  fish \
  bottom \
  direnv \
  mpv \
  git \
  ansible \
  ripgrep \
  fzf \
  procs \
  tealdeer \
  strace \
  zip \
  unrar \
  pwgen \
  exa \
  openssl \
  yt-dlp \
  tcpdump \
  nmap \
  zoxide \
  jq \
  telnet \
  httpie \
  emacs \
  net-tools \
  fontawesome-fonts \
  bind-utils \
  starship \
  netcat-openbsd \
  podman \
  hyperfine \
  dust \
  tokei \
  wl-clipboard 

