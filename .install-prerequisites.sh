#!/bin/bash

install_pkgx() {
	if which pkgx; then
		echo 'pkgx is already installed'
	else
		curl -fsSL https://pkgx.sh | bash
	fi
}

install_1password() {
	if which op; then
		echo '1Password CLI is already installed'
	else
		echo 'Installing 1Password CLI...'
		# For macOS
		if [[ "$OS" == "Darwin" ]]; then
			brew install --cask 1password 1password-cli
		fi
	fi
}

install_age() {
	if which age; then
		echo 'age is already installed'
	else
		echo 'Installing age...'
		# For macOS
		if [ "$OS" == "Darwin" ]; then
			brew install age
		# For Linux
		elif [ "$OS" == "Linux" ]; then
			apt update && apt install age
		fi
	fi
	echo 'Generating age key...'
	age-keygen | age --armor --passphrase >~/.config/chezmoi/key.txt
}

install_on_linux() {
	apt update && apt install curl git wget
	install_pkgx
	install_1password
	install_age
}

install_on_mac() {
	xcode-select --install || echo "XCode already installed"

	if [ "$(uname -m)" == "arm64" ]; then
		# Check if Rosetta is already installed
		if [ ! -f /Library/Apple/usr/share/rosetta/rosetta ]; then
			echo "Installing Rosetta 2..."
			# Run the command and capture its output and exit status
			output=$(softwareupdate --install-rosetta --agree-to-license 2>&1)
			status=$?

			# Check if the installation was successful despite potential warnings
			if [ $status -eq 0 ] || [ $output == *"finished successfully"* ]; then
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

	install_pkgx
	install_1password
	install_age
}

install_brew() {
	if which brew; then
		echo 'Homebrew is already installed'
	else
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
	fi
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
