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

install_pkgx() {
	if which pkgx >/dev/null 2>&1; then
		echo 'pkgx is already installed'
	else
		curl -fsSL https://pkgx.sh | bash
	fi
}

install_on_linux() {
	echo "Installing prerequisites for Linux..."

	# Use the SUDO function
	SUDO apt update && SUDO apt install -y curl git wget age

	# pkgx
	install_pkgx
}

install_on_mac() {
	echo "Installing prerequisites for macOS..."
	xcode-select --install || echo "XCode already installed"

	if [ "$(uname -m)" = "arm64" ]; then
		# Check if Rosetta is already installed
		if [ ! -f /Library/Apple/usr/share/rosetta/rosetta ]; then
			echo "Installing Rosetta 2..."
			# Run the command and capture its output and exit status
			output=$(softwareupdate --install-rosetta --agree-to-license 2>&1)
			status=$?

			# Check if the installation was successful despite potential warnings
			if [ $status -eq 0 ] || echo "$output" | grep -q "finished successfully"; then
				echo "Rosetta 2 installation completed successfully"
			else
				echo "Rosetta installation encountered errors but may still be functional"
				echo "Error details: $output"
			fi
		else
			echo "Rosetta 2 is already installed"
		fi
	fi

	install_brew
	eval "$(/opt/homebrew/bin/brew shellenv)"

	# pkgx
	install_pkgx

	echo "Installing prerequisites for macOS..."
	brew install age

	# Install 1Password CLI and GUI
	brew install --cask 1password 1password-cli
}

OS="$(uname -s)"
case "${OS}" in
Linux*)
	install_on_linux
	;;
Darwin*)
	install_on_mac
	;;
*)
	echo "Unsupported operating system: ${OS}"
	exit 1
	;;
esac
