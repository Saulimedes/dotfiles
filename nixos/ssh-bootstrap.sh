#!/bin/sh
# run with: curl -fsSL <raw-url> | sudo sh
# starts a throwaway sshd on port 22, no config file touched, nothing persists
set -e
grep -q '^sshd:' /etc/passwd || echo 'sshd:x:59001:59001:sshd privsep:/var/empty:/run/current-system/sw/bin/nologin' >> /etc/passwd
grep -q '^sshd:' /etc/group || echo 'sshd:x:59001:' >> /etc/group
[ -d /var/empty ] || { mkdir -p /var/empty && chmod 711 /var/empty; }
d=$(mktemp -d)
nix --extra-experimental-features "nix-command flakes" shell nixpkgs#openssh --command sh -c "
  ssh-keygen -q -t ed25519 -f '$d/hostkey' -N ''
  \$(command -v sshd) -f /dev/null -p ${PORT:-22} -h '$d/hostkey' -o PermitRootLogin=no -o PasswordAuthentication=yes -o UsePAM=no
"
echo "sshd started on port ${PORT:-22}"
