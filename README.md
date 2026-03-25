# Dotfiles

My personal dotfiles for macOS and Linux, managed with
[`chezmoi`](https://github.com/twpayne/chezmoi) and [Nix](https://nixos.org/) with
[Home Manager](https://github.com/nix-community/home-manager). Secured with
[age](https://age-encryption.org/) encryption and [1Password](https://1password.com/) integration.

## Recommendations

1. Fork the repository's main branch.
2. Follow the instructions below to set up your environment.
3. Create a personal/private branch for your personal data and push to your repo.
4. Use your personal develop branch between your setups.

## Prerequisites

- [Nix](https://nixos.org/download.html) package manager
- [Home Manager](https://github.com/nix-community/home-manager)
- [chezmoi](https://www.chezmoi.io/)
- [age](https://age-encryption.org/)
- [1Password](https://1password.com/) and
  [1Password CLI](https://1password.com/downloads/command-line/)

## Quick Install

```bash
sh -c "$(curl -fsLS https://raw.githubusercontent.com/maxclax/dotfiles/main/install.sh)"
```

## Manual Installation (without init)

1. Install chezmoi:

   ```bash
   # brew install curl wget git
   # sudo apt update && sudo apt install -y curl wget git
   sh -c "$(curl -fsLS https://raw.githubusercontent.com/maxclax/dotfiles/main/.install-prerequisites.sh)"
   ```

2. Clone repository directly:

   ```bash
   git clone https://github.com/maxclax/dotfiles.git ~/.local/share/chezmoi
   ```

3. Copy and customize reference config:

   ```bash
   # Create configuration directory
   mkdir -p ~/.config/chezmoi
   sed "s|USER_HOME|$HOME|g; s|USER|$USER|g" ~/.local/share/chezmoi/.reference-chezmoi.toml > ~/.config/chezmoi/chezmoi.toml
   ```

4. Apply configuration:

   ```bash
   chezmoi apply
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
  restic-repo="YOUR_RESTIC_BASE_REPO_PATH" \
  restic-password="YOUR_RESTIC_PASSWORD" \
  atuin-username="YOUR_ATUIN_USERNAME" \
  atuin-password="YOUR_ATUIN_PASSWORD"
```

### 3. Sign in to 1Password CLI

```bash
op signin
```

## Usage

### Daily Operations

```bash
# Apply dotfiles configuration
chezmoi apply

# See what changes would be applied
chezmoi diff

# Pull and apply updates from repository
chezmoi update

# Add new files to be managed
chezmoi add FILE

# Add encrypted files
chezmoi add --encrypt FILE
```

### Package Management with Home Manager

```bash
# See what packages will change
make hm_diff

# Update Home Manager packages and flake
make hm_update

# Commit flake.lock changes after update
make hm_commit

# List all installed packages
make hm_list

# Rollback to previous generation
make hm_rollback

# Clean old generations and garbage collect
make hm_clean
```

### System Updates

```bash
# Update all apps and packages (macOS)
make update_apps

# Update macOS system software
make update_os
```

## Features

- **Nix + Home Manager**: Declarative package management across platforms
- **Age encryption**: Encrypted sensitive data with symmetric keys
- **1Password integration**: Secure credential management via CLI
- **Git configuration**: SSH signing with automated setup
- **Container support**: Podman/Docker development environments
- **Shell configurations**: Zsh, Bash with Starship prompt and Atuin history
- **Automated backups**: Restic with scheduled daily backups via launchd
- **Cross-platform**: macOS and Linux support with platform detection

## Backup

Restic backups run daily at noon via launchd, using resticprofile for configuration.
Credentials are stored in macOS Keychain (seeded from 1Password).

Config: `~/.config/resticprofile/profiles.toml` (managed by chezmoi)
Profiles: workspace, git, managed-configs, managed-sync, extra-configs

```bash
# Run all backups
make backup_create

# List all snapshots
make backup_list
# or per profile
make restic_snapshots profile=workspace

# Initialize all repos (once per machine)
make restic_init

# Restore
make restic_restore profile=workspace dest=/tmp/restore
make restic_restore profile=workspace dest=/tmp/restore snapshot=abc1234 path=some/subdir

# Prune old snapshots
make restic_forget
```

Browse and restore via GUI:
```bash
restic-browser  # opens desktop GUI, point it at any repo path
```

## Development Environment

```bash
# Start tmux development environment
make env

# Kill tmux session
make tkill
```

## Atuin Shell History

Atuin is automatically configured through Nix. To manually log in using 1Password credentials:

```bash
atuin login --username "$(op read op://Private/chezmoi-data/atuin-username)" \
--password "$(op read op://Private/chezmoi-data/atuin-password)"
```
