# shellcheck shell=bash
# shellcheck source=/dev/null

# ----------------------------
# functions and shell-agnostic
# ----------------------------

function virtual_env_activate() {
	if [[ -n "$VIRTUAL_ENV" ]]; then
		# check the current folder belong to earlier VIRTUAL_ENV folder
		parentdir="$(dirname "$VIRTUAL_ENV")"
		if [[ "$PWD"/ != "$parentdir"/* ]]; then
			deactivate
		fi
	fi

	if [ -f .python-version ] && [ ! -d ./.venv ]; then
		uv venv
	fi

	if [[ -z "$VIRTUAL_ENV" ]]; then
		# if .venv folder is found then activate the vitualenv
		if [ -d ./.venv ] && [ -f ./.venv/bin/activate ]; then
			source ./.venv/bin/activate

			# if pyproject.toml is found then sync the virtualenv
			if [[ -f pyproject.toml ]]; then
				uv sync --all-groups
			fi
		fi
	fi
}

function node_version_manager() {
	if [[ -z "$NVMRC_PATH" ]]; then
		if [ -f .nvmrc ]; then
			nvm use
			export NVMRC_PATH=$PWD/.nvmrc
		fi
	else
		parent_nvmdir="$(dirname "$NVMRC_PATH")"
		if [[ "$PWD"/ != "$parent_nvmdir"/* ]]; then
			nvm deactivate
			export NVMRC_PATH=""
		fi
	fi
}

function zsh_completion() {
	# Makefile completion
	zstyle ':completion:*:*:make:*' tag-order 'targets'
	zstyle ':completion:*:make:*:targets' call-command true

	autoload -Uz compinit
	compinit
}

function bash_completion() {
	if [ -f "$brew_prefix/share/google-cloud-sdk" ]; then
		source "$brew_prefix/share/google-cloud-sdk/path.bash.inc"
		source "$brew_prefix/share/google-cloud-sdk/completion.bash.inc"
	fi
}

# ----------------------------
# globals
# ----------------------------

brew_prefix="$DOTFILES_BREW_PREFIX"
shell="$DOTFILES_SHELL"

# ----------------------------
# shell-agnostic configuration
# ----------------------------

if [ -f ~/.cargo/env ]; then
	source "$HOME/.cargo/env"
fi

eval "$(direnv hook $shell)"
eval "$(zoxide init $shell)"
eval "$(starship init $shell)"

# TODO need fix on pkgx side
# eval "$(pkgx --quiet dev --shellcode)"

# ----------------------------
# shell-specific configuration
# ----------------------------

# Load shell-specific completions
if [[ $shell == "zsh" ]]; then
	zsh_completion
elif [[ $shell == "bash" ]]; then
	bash_completion
fi

# Common initialization for both shells
if [ -n "$brew_prefix" ]; then
	# macOS with Homebrew
	eval "$(atuin init $shell)"

	if [[ $shell == "zsh" ]]; then
		source <(fzf --zsh)
	else
		eval "$(fzf --bash)"
	fi
else
	# Atuin initialization
	. "$HOME/.atuin/bin/env"
	# Bash-specific preexec loader
	[[ $shell == "bash" && -f ~/.bash-preexec.sh ]] && source ~/.bash-preexec.sh
	eval "$(atuin init $shell)"
	####
fi

# ----------------------------
# custom helpers
# ----------------------------

function j() {
	cd "$(find . -type d 2>/dev/null | fzf)" || return
}

function killport() {
	lsof -ti tcp:$1 | xargs kill -9
}

function extract() {
	if [ -f "$1" ]; then
		case "$1" in
		*.tar.bz2) tar xjf "$1" ;;
		*.tar.gz) tar xzf "$1" ;;
		*.bz2) bunzip2 "$1" ;;
		*.rar) unrar x "$1" ;;
		*.gz) gunzip "$1" ;;
		*.tar) tar xf "$1" ;;
		*.tbz2) tar xjf "$1" ;;
		*.tgz) tar xzf "$1" ;;
		*.zip) unzip "$1" ;;
		*.Z) uncompress "$1" ;;
		*.7z) 7z x "$1" ;;
		*) echo "'$1' cannot be extracted via extract()" ;;
		esac
	else
		echo "'$1' is not a valid file"
	fi
}

function hstart() {
	history | grep -E "^ *[0-9]+ +$1"
}

# ----------------------------------
# overrides
# ----------------------------------

function cd() {
	builtin cd "$@" || return
	virtual_env_activate
}
cd . # trigger cd overrides when shell starts

function z() {
	__zoxide_z "$@" && cd . || return
}

function zi() {
	__zoxide_zi "$@" && cd . || return

}
