#!/bin/bash
set -euo pipefail

sudo tee /etc/udev/rules.d/99-xkb-patch.rules > /dev/null << 'EOF'
# Re-apply XKB patch when NuPhy Air75 V2 keyboard is connected
ACTION=="add", SUBSYSTEM=="input", ENV{ID_VENDOR_ID}=="19f5", ENV{ID_MODEL_ID}=="3245", ENV{ID_INPUT_KEYBOARD}=="1", RUN+="/home/becker/.local/bin/xkb-patch-udev"
EOF

sudo udevadm control --reload-rules
echo "[*] XKB udev rule installed"
