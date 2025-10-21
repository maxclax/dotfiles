#!/usr/bin/env bash

set -e

# Configuration
GITHUB_USER="maxclax"
REPO_NAME="dotfiles"
BRANCH="main"
GITHUB_URL="https://github.com/${GITHUB_USER}/${REPO_NAME}.git"
RAW_URL="https://raw.githubusercontent.com/${GITHUB_USER}/${REPO_NAME}/${BRANCH}"

echo "🚀 Starting complete dotfiles installation..."
echo ""

# Step 1: Install prerequisites (Nix + Home Manager)
echo "📋 Step 1: Installing prerequisites (Nix + Home Manager)..."
echo "📥 Downloading prerequisites script..."
if command -v curl >/dev/null 2>&1; then
    curl -fsLS "${RAW_URL}/.install-prerequisites.sh" | bash
elif command -v wget >/dev/null 2>&1; then
    wget -qO- "${RAW_URL}/.install-prerequisites.sh" | bash
else
    echo "❌ Error: curl or wget required for installation"
    exit 1
fi

echo ""
echo "⏳ Waiting for prerequisites to complete..."
sleep 2

# Step 2: Source Nix profile to make commands available
echo "📋 Step 2: Setting up environment..."
if [ -f '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
elif [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
else
    echo "❌ Error: Nix profile not found. Prerequisites installation may have failed."
    exit 1
fi

# Verify Nix and Home Manager are available
if ! command -v nix >/dev/null 2>&1; then
    echo "❌ Error: Nix not found after prerequisites installation."
    exit 1
fi

if ! command -v home-manager >/dev/null 2>&1; then
    echo "❌ Error: Home Manager not found after prerequisites installation."
    exit 1
fi

# Step 3: Apply dotfiles using chezmoi via nix-shell
echo "📋 Step 3: Applying dotfiles configuration..."

# Check if chezmoi config already exists (container case)
if [ -f ~/.config/chezmoi/chezmoi.toml ]; then
    echo "🔧 Using existing chezmoi configuration..."
    nix-shell -p chezmoi git --run "
        chezmoi apply && \
        echo '✅ Dotfiles applied successfully!'
    "
else
    echo "🔧 Initializing new chezmoi configuration..."
    nix-shell -p chezmoi git --run "
        chezmoi init --branch ${BRANCH} --apply ${GITHUB_URL} && \
        echo '✅ Dotfiles applied successfully!'
    "
fi

if [ $? -ne 0 ]; then
    echo "❌ Error: Failed to apply dotfiles"
    exit 1
fi

# Step 4: Set up Home Manager
echo ""
echo "📋 Step 4: Setting up Home Manager..."
echo "🔧 Enabling Nix experimental features..."

# Enable experimental features for this session
export NIX_CONFIG="experimental-features = nix-command flakes"

if home-manager switch --flake ~/.config/home-manager-flake --extra-experimental-features "nix-command flakes" -b backup; then
    echo "✅ Home Manager setup complete!"
else
    echo "❌ Error: Home Manager setup failed"
    exit 1
fi

echo ""
echo "🎉 Complete installation finished!"
echo ""
echo "Your dotfiles have been installed and configured!"
echo "Please restart your shell or run:"
echo "  source ~/.zshrc  # or ~/.bashrc"
echo ""
echo "Available commands:"
echo "  make hm_update    # Update packages"
echo "  make hm_list      # List installed packages"
echo "  chezmoi apply     # Apply dotfiles changes"
