#!/bin/sh
set -e
mkdir -p /tmp/logserve
{
  echo "=== /etc/pam.d/sshd ==="
  cat /etc/pam.d/sshd
  echo "=== ephemeral linux-pam path ==="
  nix --extra-experimental-features "nix-command flakes" build --no-link --print-out-paths nixpkgs#linux-pam
  echo "=== system linux-pam in closure ==="
  readlink -f /run/current-system/sw/bin/su 2>/dev/null
  ls -la /run/current-system/sw/bin/sshd 2>&1 || echo "no system sshd binary"
  echo "=== nix registry ==="
  cat /etc/nix/registry.json 2>&1
  echo "=== journal for pam ==="
  journalctl -n 100 --no-pager 2>&1 | grep -iE 'pam|sshd|becker' | tail -40
} > /tmp/logserve/diag3.log 2>&1
cat /tmp/logserve/diag3.log
pkill -f 'http.server 8900' 2>/dev/null || true
cd /tmp/logserve
nix --extra-experimental-features "nix-command flakes" shell nixpkgs#python3 --command python3 -m http.server 8900 --bind 0.0.0.0 >/tmp/logserve.out 2>&1 &
disown
sleep 1
echo "diag3 served on port 8900"
