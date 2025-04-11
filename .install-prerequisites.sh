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
		# For Linux
		elif [[ "$OS" == "Linux" ]]; then
			sudo apt update && sudo apt install 1password 1password-cli
		fi
	fi
}

install_on_linux() {
	install_pkgx
	install_1password
}

install_on_mac() {
	xcode-select --install || echo "XCode already installed"

	if [[ "$(uname -m)" == "arm64" ]]; then
		softwareupdate --install-rosetta --agree-to-license
	fi

	install_brew
	eval "$(/opt/homebrew/bin/brew shellenv)"

	install_pkgx
	install_1password
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
