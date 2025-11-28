#!/usr/bin/env bash

# Clean up existing chezmoi installation to start fresh
echo "Cleaning up existing chezmoi installation..."
rm -rf ~/.local/share/chezmoi

# SUDO function to handle command execution with proper privileges
SUDO() {
	if command -v sudo >/dev/null 2>&1; then
		sudo "$@"
	elif [ "$(id -u)" -eq 0 ]; then
		"$@"
	else
		echo "Error: 'sudo' command not found and not running as root."
		echo "Please either:"
		echo "1. Install sudo"
		echo "2. Run this script as root"
		echo "3. Manually install the required packages"
		exit 1
	fi
}

install_nix_and_home_manager() {
	if command -v nix >/dev/null 2>&1; then
		echo 'Nix is already installed'
	else
		if [ "$(uname)" = "Darwin" ]; then
			echo 'Installing Nix using nix-darwin installer...'
			curl -L https://nixos.org/nix/install | sh -s -- --darwin-use-unencrypted-nix-store-volume
		else
			echo 'Installing Nix using default installer...'
			curl -L https://nixos.org/nix/install | sh
		fi
	fi

    if [ -f '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
        . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
    elif [ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
        . "$HOME/.nix-profile/etc/profile.d/nix.sh"
    fi

	# Remove specific conflicting packages before Home Manager installation (Linux only)
	if [ "$(uname)" = "Linux" ]; then
		echo 'Checking for specific package conflicts...'
		if command -v nix-env >/dev/null 2>&1; then
			# Only remove packages that are known to conflict with home-manager-path
			for pkg in man-db openssh wget curl git bash coreutils; do
				if nix-env -q 2>/dev/null | grep -q "^$pkg"; then
					echo "Removing conflicting package: $pkg"
					nix-env -e "$pkg" 2>/dev/null || true
				fi
			done
		fi
	fi

	if command -v home-manager >/dev/null 2>&1; then
		echo 'Home Manager is already installed'
	else
		echo 'Installing Home Manager...'

		# Add home-manager channel
		nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
		nix-channel --update

		# Enable flakes for Home Manager installation
		export NIX_CONFIG="experimental-features = nix-command flakes"

		# Install Home Manager
		nix-shell '<home-manager>' -A install

		echo 'Home Manager installed successfully!'
	fi

}

OS="$(uname -s)"
case "${OS}" in
Linux*)
	echo "Installing prerequisites for Linux..."
	install_nix_and_home_manager

	;;
Darwin*)
	echo "Installing prerequisites for macOS..."
	xcode-select --install || echo "XCode already installed"

	# Install Homebrew first (required by nix-darwin config)
	if ! command -v brew >/dev/null 2>&1; then
		echo "Installing Homebrew..."
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

		# Add Homebrew to PATH
		if [ -f "/opt/homebrew/bin/brew" ]; then
			eval "$(/opt/homebrew/bin/brew shellenv)"
		elif [ -f "/usr/local/bin/brew" ]; then
			eval "$(/usr/local/bin/brew shellenv)"
		fi
	else
		echo "Homebrew already installed"
	fi

	echo "Installing Nix and Home Manager..."
	install_nix_and_home_manager

	;;
*)
	echo "Unsupported operating system: ${OS}"
	exit 1
	;;
esac

echo ""
echo "🎉 Prerequisites installation complete!"
echo ""
