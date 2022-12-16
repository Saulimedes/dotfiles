#!/bin/sh
sudo apt install curl --yes
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" --no-update --yes
sudo apt update
sudo apt install -y \
  docker-ce \
  docker-ce-cli \
  containerd.io \
  docker-compose \
  docker-compose-plugin

sudo groupadd docker -f
sudo usermod -aG docker $USER
