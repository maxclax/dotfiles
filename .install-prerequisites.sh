#!/usr/bin/env bash

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

install_brew() {
	if which brew >/dev/null 2>&1; then
		echo 'Homebrew is already installed'
	else
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
	fi
}

install_nix() {
	if command -v nix >/dev/null 2>&1; then
		echo 'Nix is already installed'
	else
		echo 'Installing Nix using OFFICIAL installer...'
		curl -L https://nixos.org/nix/install | sh
		# Source Nix for current session
		if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
			. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
		fi
	fi
}

OS="$(uname -s)"
case "${OS}" in
Linux*)
	echo "Installing prerequisites for Linux..."
	install_nix

	;;
Darwin*)
	echo "Installing prerequisites for macOS..."
	xcode-select --install || echo "XCode already installed"

	echo "Installing prerequisites for macOS..."
	install_nix

	install_brew
	eval "$(/opt/homebrew/bin/brew shellenv)"
	brew install --cask 1password

	;;
*)
	echo "Unsupported operating system: ${OS}"
	exit 1
	;;
esac
