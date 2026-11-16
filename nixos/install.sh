#!/usr/bin/env bash
# usage (live ISO, as root): ./install.sh HOST DISK    e.g. ./install.sh notebook /dev/nvme0n1
set -euo pipefail

host=${1:?usage: ./install.sh HOST DISK}
disk=${2:?usage: ./install.sh HOST DISK}
here=$(cd "$(dirname "$0")" && pwd)
luks=/tmp/luks-password
userpw=/tmp/user-password
export NIX_CONFIG='experimental-features = nix-command flakes'

[[ $EUID -eq 0 ]] || { echo "run as root" >&2; exit 1; }
[[ -b $disk ]] || { echo "$disk is not a block device" >&2; exit 1; }
[[ -f $here/hosts/$host/disko.nix ]] || { echo "host $host has no disko.nix" >&2; exit 1; }
[[ -z $(lsblk -nro MOUNTPOINTS "$disk" | tr -d '[:space:]') ]] || { echo "$disk has mounted partitions" >&2; exit 1; }
curl -fsI https://cache.nixos.org >/dev/null || { echo "no network" >&2; exit 1; }

identity=/tmp/chezmoi.toml
trap 'shred -u "$luks" "$userpw" "$identity" 2>/dev/null || true' EXIT
umask 077

ask() {
  [[ -s $1 ]] && return
  local a b
  while :; do
    read -rsp "$2: " a; echo
    read -rsp "$2 (again): " b; echo
    [[ -n $a && $a == "$b" ]] && break
    echo "empty or different, again" >&2
  done
  printf '%s' "$a" > "$1"
}

lsblk -o NAME,PATH,SIZE,MODEL,TRAN "$disk"
read -rp "Erase everything on $disk? Type the device path to confirm: " answer
[[ $answer == "$disk" ]] || { echo aborted >&2; exit 1; }

ask "$luks" "LUKS passphrase"
ask "$userpw" "Password for becker"

real=$(readlink -f "$disk")
dev=$disk
for link in /dev/disk/by-id/*; do
  [[ $link =~ -part[0-9]+$ ]] && continue
  if [[ $(readlink -f "$link") == "$real" ]]; then dev=$link; break; fi
done
printf '{\n  disko.devices.disk.main.device = "%s";\n}\n' "$dev" > "$here/hosts/$host/disk-device.nix"

nix run github:nix-community/disko/latest -- \
  --mode destroy,format,mount --yes-wipe-all-disks --flake "path:$here#$host"
shred -u "$luks"

nixos-generate-config --no-filesystems --root /mnt --show-hardware-config \
  > "$here/hosts/$host/hardware-configuration.nix"

mkdir -p /mnt/etc/nixos
cp -a "$here"/. /mnt/etc/nixos/

nixos-install --root /mnt --no-root-passwd --flake "path:/mnt/etc/nixos#$host"

printf 'becker:%s\n' "$(cat "$userpw")" | nixos-enter --root /mnt -c chpasswd

if [[ -f $identity ]]; then
  nixos-enter --root /mnt -c "runuser -u becker -- sh -c 'mkdir -p /home/becker/.config/chezmoi && cat > /home/becker/.config/chezmoi/chezmoi.toml'" < "$identity"
fi

echo "done. hosts/$host/hardware-configuration.nix was generated in $here, copy it back into the repo, then reboot."
