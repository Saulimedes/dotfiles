#!/bin/bash
# Autofs config for Unraid NAS shares - reruns when this file changes
set -euo pipefail

if [[ ! -f /etc/gentoo-release ]]; then
    echo "[!] Not Gentoo, skipping"
    exit 0
fi

sudo tee /etc/autofs/auto.unraid > /dev/null << 'EOF'
movies      -fstype=cifs,guest,uid=1000,gid=1000,iocharset=utf8,file_mode=0644,dir_mode=0755  ://192.168.178.27/Movies
tvshows     -fstype=cifs,guest,uid=1000,gid=1000,iocharset=utf8,file_mode=0644,dir_mode=0755  ://192.168.178.27/TV\040Shows
anime       -fstype=cifs,guest,uid=1000,gid=1000,iocharset=utf8,file_mode=0644,dir_mode=0755  ://192.168.178.27/Anime
music       -fstype=cifs,guest,uid=1000,gid=1000,iocharset=utf8,file_mode=0644,dir_mode=0755  ://192.168.178.27/Music
ebooks      -fstype=cifs,guest,uid=1000,gid=1000,iocharset=utf8,file_mode=0644,dir_mode=0755  ://192.168.178.27/Ebooks
documents   -fstype=cifs,guest,uid=1000,gid=1000,iocharset=utf8,file_mode=0644,dir_mode=0755  ://192.168.178.27/Documents
audiobooks  -fstype=cifs,guest,uid=1000,gid=1000,iocharset=utf8,file_mode=0644,dir_mode=0755  ://192.168.178.27/Audiobooks
stuff       -fstype=cifs,guest,uid=1000,gid=1000,iocharset=utf8,file_mode=0644,dir_mode=0755  ://192.168.178.27/Stuff
EOF

# Idempotently add the unraid map to auto.master
if ! grep -q '/mnt/unraid' /etc/autofs/auto.master; then
    echo -e "\n# Unraid NAS shares" | sudo tee -a /etc/autofs/auto.master > /dev/null
    echo '/mnt/unraid  /etc/autofs/auto.unraid  --timeout=300 --ghost' | sudo tee -a /etc/autofs/auto.master > /dev/null
fi

sudo mkdir -p /mnt/unraid
sudo rc-service autofs restart 2>/dev/null || true

echo "[*] Samba/autofs mounts configured"
