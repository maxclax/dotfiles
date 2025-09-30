#!/usr/bin/env bash

set -e

echo "🚀 Starting complete dotfiles installation..."
echo ""

# Get script directory
script_dir="$(cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P)"

# Step 1: Install prerequisites (Nix + Home Manager)
echo "📋 Step 1: Installing prerequisites (Nix + Home Manager)..."
if [ -f "${script_dir}/.install-prerequisites.sh" ]; then
    bash "${script_dir}/.install-prerequisites.sh"
else
    echo "❌ Error: .install-prerequisites.sh not found in ${script_dir}"
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
nix-shell -p chezmoi --run "
    chezmoi init --apply --source='${script_dir}' && \
    echo '✅ Dotfiles applied successfully!'
"

if [ $? -ne 0 ]; then
    echo "❌ Error: Failed to apply dotfiles"
    exit 1
fi

# Step 4: Set up Home Manager
echo ""
echo "📋 Step 4: Setting up Home Manager..."
if home-manager switch --flake ~/.config/home-manager-flake; then
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
