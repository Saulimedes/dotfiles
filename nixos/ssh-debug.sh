#!/bin/sh
set -e
pkill -f 'sshd -f /dev/null' 2>/dev/null || true
sleep 1
grep -q '^sshd:' /etc/passwd || echo 'sshd:x:59001:59001:sshd privsep:/var/empty:/run/current-system/sw/bin/nologin' >> /etc/passwd
grep -q '^sshd:' /etc/group || echo 'sshd:x:59001:' >> /etc/group
[ -d /var/empty ] || { mkdir -p /var/empty && chmod 711 /var/empty; }
d=$(mktemp -d)
nix --extra-experimental-features "nix-command flakes" shell nixpkgs#openssh --command sh -c "
  ssh-keygen -q -t ed25519 -f '$d/hostkey' -N ''
  \$(command -v sshd) -f /dev/null -p 22 -h '$d/hostkey' -o PermitRootLogin=no -o PasswordAuthentication=yes -o KbdInteractiveAuthentication=no -o UsePAM=yes -o LogLevel=DEBUG3 -o SyslogFacility=USER -E /tmp/sshd-debug.log
"
sleep 1
mkdir -p /tmp/logserve
ln -sf /tmp/sshd-debug.log /tmp/logserve/sshd-debug.log
pkill -f 'http.server 8900' 2>/dev/null || true
cd /tmp/logserve
nix --extra-experimental-features "nix-command flakes" shell nixpkgs#python3 --command python3 -m http.server 8900 --bind 0.0.0.0 >/tmp/logserve.out 2>&1 &
disown
sleep 1
echo "sshd started on port 22, debug log served on port 8900"
