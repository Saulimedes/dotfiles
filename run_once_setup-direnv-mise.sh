#!/bin/bash
# Wire up direnv's `use mise` - without this, `use mise` in a .envrc fails
# silently with "use_mise: command not found" and no env vars load.
set -euo pipefail

if command -v mise >/dev/null && command -v direnv >/dev/null; then
    mkdir -p ~/.config/direnv
    mise direnv > ~/.config/direnv/direnvrc
fi
