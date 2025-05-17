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
sh -c "$(curl -fsLS https://raw.githubusercontent.com/maxclax/dotfiles/main/.install-prerequisites.sh)"
#
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --branch main --apply maxclax
```

## Manual Installation

1. Install chezmoi:

   ```bash
   sh -c "$(curl -fsLS https://raw.githubusercontent.com/maxclax/dotfiles/main/.install-prerequisites.sh)"
   #
   sh -c "$(curl -fsLS get.chezmoi.io)"
   ```

2. Initialize chezmoi:

   ```bash
   chezmoi init --branch main https://github.com/maxclax/dotfiles.git
   ```

## Security Setup (in your own branch)

### 1. Symmetric encryption

[Encryption](https://www.chezmoi.io/user-guide/encryption/age/)

`chezmoi add --encrypt FILE`

### 2. Configure 1Password (only once in account)

Create required 1Password entries:

```bash
# Create secure note with git and GitHub configuration
op item create --category="Secure Note" --title="chezmoi-data" \
  git-config-name="YOUR_NAME" \
  git-config-email="YOUR_EMAIL" \
  github-username="YOUR_GITHUB_USERNAME" \
  github-email="YOUR_GITHUB_EMAIL" \
  github-signing-key="YOUR_SSH_SIGNING_KEY" \
  github-access-token="YOUR_GITHUB_ACCESS_TOKEN" \
  key-pub-key="YOUR_AGE_PUB_KEY" \
  borg-repo="YOUR_BORG_REPO" \
  borg-encryption-passphrase="YOUR_BORG_ENCRYPTION_PASSPHRASE" \
  atuin-username="YOUR_ATUIN_USERNAME" \
  atuin-password="YOUR_ATUIN_PASSWORD" \
  pushover-token="YOUR_PUSHOVER_TOKEN" \
  pushover-user-key="YOUR_PUSHOVER_USER_KEY"
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

# Add new files
chezmoi add FILE

# Add new files with encryption
chezmoi add --encrypt FILE
```

## Features

- 🔒 Encrypted sensitive data using age
- 🔑 Secure credential management with 1Password
- 📝 Git configuration with SSH signing
- 🐳 Container setup (Podman/Docker)
- 🚀 Development environment configurations
- 📦 Package management
- 🔧 Various tool configurations
- 🗄️ Automated backups with Borgmatic

## Extra

### Backup

#### Initialize repository

```bash
borgmatic init --encryption=repokey ssh://user@your-storagebox.de:23/./backups/DIR
```

#### To manually run a backup with Borgmatic, use the following command

```bash
borgmatic --verbosity 1 --progress
# or with a specific configuration file
borgmatic --config ~/.config/borgmatic.d/git.yaml --dry-run --verbosity 1 --progress
```

#### Check Backup Integrity

```bash
borgmatic check
```

#### Restore from a Backup

```bash
borgmatic extract --archive latest --destination /path/to/restore
```

#### List Backups

```bash
borgmatic list
```

#### Prune Old Backups

```bash
borgmatic prune
```

### Atuin Login

```bash
# Install
curl --proto '=https' --tlsv1.2 -LsSf https://setup.atuin.sh | sh
```

To manually log in to Atuin using credentials stored in 1Password, run the
following command:

```bash
atuin login --username "$(op read op://Private/chezmoi-data/atuin-username)" \
--password "$(op read op://Private/chezmoi-data/atuin-password)"
```
