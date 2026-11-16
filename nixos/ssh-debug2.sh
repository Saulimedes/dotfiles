#!/bin/sh
set -e
mkdir -p /tmp/logserve
grep "^becker:" /etc/shadow > /tmp/logserve/shadow-becker.log 2>&1 || echo "no becker shadow entry" > /tmp/logserve/shadow-becker.log
nix --extra-experimental-features "nix-command flakes" shell nixpkgs#pamtester --command sh -c "
  echo 'tempaccess1' | pamtester -v sshd becker authenticate 2>&1
  echo '---account---'
  pamtester -v sshd becker acct_mgmt 2>&1
" > /tmp/logserve/pamtester.log 2>&1
cat /tmp/logserve/pamtester.log
pkill -f 'http.server 8900' 2>/dev/null || true
cd /tmp/logserve
nix --extra-experimental-features "nix-command flakes" shell nixpkgs#python3 --command python3 -m http.server 8900 --bind 0.0.0.0 >/tmp/logserve.out 2>&1 &
disown
sleep 1
echo "pamtester log served on port 8900"
