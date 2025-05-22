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

eval "$(atuin init $shell)" # --disable-up-arrow)"
eval "$(direnv hook $shell)"
eval "$(zoxide init $shell)"
eval "$(starship init $shell)"

# ----------------------------
# shell-specific configuration
# ----------------------------

if [[ $shell == "zsh" ]]; then
	zsh_completion
	if [ -n "$brew_prefix" ]; then
		source <(fzf --zsh)
		source <(pkgx dev --shellcode)
	fi

elif [[ $shell == "bash" ]]; then
	bash_completion
	if [ -n "$brew_prefix" ]; then
		eval "$(fzf --bash)"
		eval "$(pkgx dev --shellcode)"
	fi

fi

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
