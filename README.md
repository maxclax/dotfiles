# Dotfiles

My personal dotfiles for macOS and Linux, managed with
[`chezmoi`](https://github.com/twpayne/chezmoi) and secured with
[age](https://age-encryption.org/) encryption.

## Recommendations

1. Fork the repository's main branch.
2. Follow the instructions below to set up your environment.
3. Create a personal/me/private branch, and all personal data needs to be stored
   in this new branch. Then push this branch to your repo.
4. Use your personal develop branch between your setups.

## Prerequisites

- [chezmoi](https://www.chezmoi.io/)
- [age](https://age-encryption.org/)
- [1Password](https://1password.com/) and
  [1Password CLI](https://1password.com/downloads/command-line/)

## Quick Install

```bash
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --branch main --apply maxclax
```

## Manual Installation

1. Install chezmoi:

```bash
sh -c "$(curl -fsLS get.chezmoi.io)"
```

2. Initialize chezmoi:

```bash
chezmoi init --branch main https://github.com/maxclax/dotfiles.git
```

## Security Setup (in your own branch)

### 1. Generate age Key (First Time Setup)

```bash
chezmoi cd ~
age-keygen | age --armor --passphrase > key.txt.age
```

Next use to add data in chezmoi.toml
[Encryption](https://www.chezmoi.io/user-guide/frequently-asked-questions/encryption/)

### 2. Configure 1Password (only once in account)

Create required 1Password entries:

```bash
# Create secure note with git and GitHub configuration
op item create --category="Secure Note" --title="chezmoi-data" \
  git-config-name="YOUR_NAME" \
  git-config-email="YOUR_EMAIL" \
  github-username="YOUR_GITHUB_USERNAME" \
  github-email="YOUR_GITHUB_EMAIL" \
  github-signing-key="YOUR_SSH_SIGNING_KEY"
```

### 3. Sign in to 1Password CLI

```bash
op signin
```

## Usage

### Apply Configuration

```bash
chezmoi apply
```

### Update Configuration

```bash
# Pull and apply updates
chezmoi update

# Edit configuration
chezmoi edit

# See pending changes
chezmoi diff
```

## Features

- 🔒 Encrypted sensitive data using age
- 🔑 Secure credential management with 1Password
- 📝 Git configuration with SSH signing
- 🐳 Container setup (Podman/Docker)
- 🚀 Development environment configurations
- 📦 Package management
- 🔧 Various tool configurations