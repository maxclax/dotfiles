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

  # Create venv if needed
  if [ -f .python-version ] && [ ! -d ./.venv ]; then
    if [ -f pyproject.toml ] && grep -q "tool.poetry" pyproject.toml; then
      poetry env use $(cat .python-version)
    else
      uv venv
    fi
  fi

  if [[ -z "$VIRTUAL_ENV" ]]; then
    # Try poetry first if pyproject.toml exists with poetry config
    if [ -f pyproject.toml ] && grep -q "tool.poetry" pyproject.toml; then
      poetry shell
    # Fall back to regular venv activation
    elif [ -d ./.venv ] && [ -f ./.venv/bin/activate ]; then
      source ./.venv/bin/activate

      # Sync dependencies if pyproject.toml exists
      if [[ -f pyproject.toml ]]; then
        if grep -q "tool.poetry" pyproject.toml; then
          poetry install --sync --all-extras
        else
          uv sync --all-groups
        fi
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
  if [ -n "$brew_prefix" ]; then
    export FPATH=$brew_prefix/share/zsh/site-functions:$FPATH

    source "$brew_prefix/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
    source "$brew_prefix/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
  fi

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

if [ -n "$brew_prefix" ]; then
  # TODO: evaluate whether pkgx can replace nvm
  # source "$brew_prefix/opt/nvm/nvm.sh"

  eval "$(atuin init $shell --disable-up-arrow)"
  eval "$(direnv hook $shell)"
  eval "$(zoxide init $shell)"
  eval "$(starship init $shell)"

fi

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
  # node_version_manager  # TODO: with pkgx, maybe nvm is no longer needed?
}
cd . # trigger cd overrides when shell starts

function z() {
  __zoxide_z "$@" && cd . || return
}

function zi() {
  __zoxide_zi "$@" && cd . || return

}
