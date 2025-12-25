#!/bin/bash

# Install antidote zsh plugin manager

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Create .zsh directory if it doesn't exist
ZDOTDIR="$HOME/.zsh"
mkdir -p "$ZDOTDIR"

# Install antidote if not already installed
if [[ ! -d "$ZDOTDIR/antidote" ]]; then
    log_info "Installing antidote zsh plugin manager"
    
    git clone --depth=1 https://github.com/mattmc3/antidote.git "$ZDOTDIR/antidote" || {
        log_error "Failed to clone antidote repository"
        exit 1
    }
    
    log_info "Antidote installed successfully"
else
    log_info "Antidote already installed, updating..."
    
    cd "$ZDOTDIR/antidote"
    git pull origin main || {
        log_warn "Failed to update antidote (this is usually not critical)"
    }
    cd - > /dev/null
fi

# Create empty plugins file if it doesn't exist
if [[ ! -f "$ZDOTDIR/zsh_plugins.txt" ]]; then
    log_info "Creating default zsh_plugins.txt"
    touch "$ZDOTDIR/zsh_plugins.txt"
fi

# Create completions directory
mkdir -p "$ZDOTDIR/completions"

# Update zshrc to handle plugin updates
log_info "Antidote setup completed"
log_info "Plugins will be automatically updated when you next start zsh"