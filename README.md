# Dotfiles

My personal dotfiles, managed with
[`chezmoi`](https://github.com/twpayne/chezmoi) and secured with
[age](https://age-encryption.org/) encryption.

## Prerequisites

- [chezmoi](https://www.chezmoi.io/)
- [age](https://age-encryption.org/)
- [1Password](https://1password.com/) and
  [1Password CLI](https://1password.com/downloads/command-line/)

## Quick Install

```bash
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply maxclax
```

## Manual Installation

1. Install chezmoi:

```bash
sh -c "$(curl -fsLS get.chezmoi.io)"
```

2. Initialize chezmoi:

```bash
chezmoi init https://github.com/maxclax/dotfiles.git
```

## Security Setup

### 1. Generate age Key (First Time Setup)

```bash
chezmoi cd ~
age-keygen | age --armor --passphrase > key.txt.age
```

Next use to add data in chezmoi.toml
[Encryption](https://www.chezmoi.io/user-guide/frequently-asked-questions/encryption/)

### 2. Configure 1Password

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

### Crontab Configuration

To customize your system's crontab configuration:

1. Switch to the develop branch
2. Edit the `dot_crontab` file in your repository
3. Add your desired cron jobs following standard crontab syntax
4. Apply changes using `chezmoi apply`

Example `dot_crontab` content:

```bash
# m h dom mon dow command
0 * * * * /path/to/hourly/script
0 0 * * * /path/to/daily/backup
```

## Features

- 🔒 Encrypted sensitive data using age
- 🔑 Secure credential management with 1Password
- 📝 Git configuration with SSH signing
- 🐳 Container setup (Podman/Docker)
- 🚀 Development environment configurations
- 📦 Package management
- 🔧 Various tool configurations
