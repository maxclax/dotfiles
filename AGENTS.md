# AGENTS.md

This file provides guidance to AI coding agents (Claude Code, Grok Build, Cursor, etc.) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository managed with [chezmoi](https://chezmoi.io) for cross-platform configuration management. The repository uses Nix with Home Manager for package management and includes encrypted secrets management via age and 1Password integration.

## Key Commands

### Chezmoi Operations
```bash
# Apply dotfiles configuration
chezmoi apply

# See what changes would be applied
chezmoi diff

# Update from source repository
chezmoi update

# Add new files to be managed
chezmoi add <file>

# Add encrypted files
chezmoi add --encrypt <file>
```

### Home Manager Package Management
```bash
# See what packages will change
make hm_diff

# Update Home Manager packages and flake
make hm_update

# Commit flake.lock changes after update
make hm_commit

# List installed packages
make hm_list

# Rollback to previous generation
make hm_rollback

# Clean old generations
make hm_clean
```

### System Updates (macOS)
```bash
# Update all apps and packages
make update_apps

# Update macOS system software
make update_os
```

### Backup Operations
Backups use restic via resticprofile; one repository per profile
(`workspace`, `git`, `managed-configs`, `managed-sync`, `ai-history`,
`extra-configs`). Config: `private_dot_config/resticprofile/profiles.toml.tmpl`.
```bash
# Sync app configs to extra-configs, then back up every profile
make backup_create

# List snapshots (all profiles, or one)
make backup_list
make restic_snapshots profile=workspace

# Restore
make restic_restore profile=<name> dest=<path> [snapshot=latest] [path=<subpath>]

# Mount a repo to browse it
make restic_browse [profile=workspace]
```

### Container Management
```bash
# Start Tor proxy container
make proxy_start

# Stop Tor proxy
make torproxy_stop

# Clean up all containers (targets are prefixed with the engine)
make podman_rm_a

# Remove dangling <none> images
make podman_rm_none_images
```

### Development Environment
```bash
# Start tmux development environment
make env

# Kill tmux session
make tkill
```

## Architecture

### Configuration Structure

- **`.chezmoi.toml.tmpl`**: Main chezmoi configuration with user prompts and data templating
- **`Makefile.tmpl`**: Templated Makefile with all management commands
- **`private_dot_config/`**: Configuration files for various applications
  - **`home-manager-flake/`**: Home Manager flake configuration
    - `flake.nix.tmpl`: Home Manager flake definition
    - `home.nix.tmpl`: Main Home Manager configuration
    - `packages.nix.tmpl`: Package definitions
    - `programs/`: Modular program configurations (zsh, etc.)

### Home Manager Configuration

The Home Manager flake setup is modularized:
- `flake.nix` defines the Home Manager flake inputs and outputs
- `home.nix` imports modular program configurations
- `packages.nix` contains all package definitions organized by category
- `programs/` directory contains individual program configurations

### Templating System

Files with `.tmpl` extension use chezmoi's Go templating:
- Variables from `.chezmoi.toml.tmpl` are available in templates
- Conditional configurations based on OS, hostname, and user preferences
- Integration with 1Password for secure credential management

**When creating `.tmpl` files, always include the correct Emacs mode line on the first line:**
- `*.el.tmpl` → `;;; filename.el -*- mode: emacs-lisp; lexical-binding: t; -*-`
- `*.sh.tmpl` → `# filename.sh -*- mode: sh; lexical-binding: t; -*-`
- `*.nix.tmpl` → `# -*- mode: nix; -*-`
- `*.yml.tmpl` / `*.yaml.tmpl` → `# -*- mode: yaml; -*-`
- `*.toml.tmpl` → `# -*- mode: conf-toml; -*-`
- `*.ini.tmpl` → `; -*- mode: conf-windows; -*-` (INI comments are `;`; `conf-unix`
  would treat those lines as data and let apostrophes open strings)

### External Dependencies

External repositories are managed via `.chezmoiexternal.yaml.tmpl`:
- tmux configuration from gpakosz/.tmux
- Emacs configurations (Doom, Purcell, etc.)
- Alacritty themes

### Security

- Age encryption for sensitive files (identity: `~/.ssh/dotfiles`)
- 1Password CLI integration for secure credential storage
- Git signing configuration with SSH keys
- Encrypted backup system with restic (resticprofile)

## Development Workflow

1. **Initialize**: Use `chezmoi init` to set up on new machines
2. **Configure**: Edit templates in the source directory
3. **Test**: Use `chezmoi diff` to preview changes
4. **Apply**: Use `chezmoi apply` to deploy changes
5. **Update Packages**: Use `make hm_update` for Home Manager packages
6. **Commit**: Use `make hm_commit` to commit flake.lock changes

## Platform Support

- **macOS**: Full support with Homebrew and App Store integration
- **Linux**: Supported with platform-specific package management
- **Ephemeral environments**: Automatic detection for containers, VMs, etc.

## Key Features

- Cross-platform dotfile management
- Declarative package management with Home Manager
- Encrypted secrets with age and 1Password
- Automated backups with restic
- Multiple editor configurations (Emacs distributions)
- Container development environment support
- Git configuration with SSH signing
