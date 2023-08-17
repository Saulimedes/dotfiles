#!/bin/sh
sudo zypper refresh
sudo zypper -n install curl
sudo rpm --import https://dl.google.com/linux/linux_signing_key.pub
sudo zypper addrepo http://dl.google.com/linux/chrome/rpm/stable/x86_64 Google-Chrome
sudo zypper install -n chrome-browser
