#!/bin/sh

sudo apt update -y
sudo apt upgrade -y
sudo DEBIAN_FRONTEND=noninteractive  apt install -y \
  fish \
  nala \
  fd-find \
  direnv \
  htop \
  mpv \
  neovim \
  git \
  xdg-user-dirs \
  wget \
  ansible \
  ripgrep \
  fzf \
  tldr \
  strace \
  zip \
  p7zip \
  unzip \
  unrar \
  pwgen \
  exa \
  openssl \
  tmux \
  yt-dlp \
  fonts-font-awesome \
  fonts-powerline \
  dnsutils \
  tcpdump \
  zoxide \
  nmap \
  curl \
  netcat-openbsd \
  jq \
  telnet \
  tshark \
  wireguard  \
  fonts-firacode  \
  bat  \
  traceroute \
  nodejs \
  npm \
  rsync

sudo apt-get autoremove -y
sudo apt-get autoclean -y
mkdir -p ~/.local/bin
if [ ! -f ~/.local/bin/fd ]; then
  ln -s $(which fdfind) ~/.local/bin/fd || true
fi

if [ ! -f ~/.local/bin/bat ]; then
  ln -s $(which batcat) ~/.local/bin/bat || true
fi
