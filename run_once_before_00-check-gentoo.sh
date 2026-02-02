#!/bin/bash
# Verify this is a Gentoo system before proceeding

if [[ ! -f /etc/gentoo-release ]]; then
    echo "========================================"
    echo "WARNING: Non-Gentoo System Detected"
    echo "========================================"
    echo ""
    echo "This chezmoi configuration is designed for Gentoo Linux."
    echo "Some features may not work correctly on other distributions."
    echo ""
    echo "Detected OS: $(cat /etc/os-release 2>/dev/null | grep -E '^PRETTY_NAME=' | cut -d= -f2 | tr -d '"')"
    echo ""
    echo "If you want to continue anyway, the dotfiles will be applied"
    echo "but the run_once scripts may fail or behave unexpectedly."
    echo ""
fi
