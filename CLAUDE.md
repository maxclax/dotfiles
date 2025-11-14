# CLAUDE.md

=========================================================================================================

## Claude Code Instructions

**IMPORTANT: Always check ROADMAP.org file first** based on the project context:

### Workflow:
2. **Read the appropriate ROADMAP.org** to understand priorities and context
3. **Update task statuses** as you progress:
   - Change `TODO` to `NEXT` when starting work
   - Mark as `DONE` when completed and manually add CLOSED timestamp like:
     ```
     * DONE [#A] Task name
       CLOSED: [2025-01-11 Sat 14:30]
     ```
   - Move completed items under "Recently Completed" section
   - Add new tasks as they arise during development

### Priority System:
- `[#A]` = High priority (work on first)
- `[#B]` = Medium priority
- `[#C]` = Low priority

=========================================================================================================

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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
```bash
# Create backup with Borgmatic
make backup_create

# List available backups
make backup_list

# Restore from backup
make backup_restore repo=<path> archive=<name> dest=<destination>
```

### Container Management
```bash
# Start Tor proxy container
make proxy_start

# Stop Tor proxy
make torproxy_stop

# Clean up all containers
make rm_a
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
    - `programs/`: Modular program configurations (zsh, vim, etc.)

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

### External Dependencies

External repositories are managed via `.chezmoiexternal.yaml.tmpl`:
- tmux configuration from gpakosz/.tmux
- Vim/Neovim distributions (LunarVim, LazyVim)
- Emacs configurations (Doom, Prelude, etc.)
- Alacritty themes

### Security

- Age encryption for sensitive files (identity: `~/.ssh/dotfiles`)
- 1Password CLI integration for secure credential storage
- Git signing configuration with SSH keys
- Encrypted backup system with Borgmatic

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
- Automated backups with Borgmatic
- Multiple editor configurations (Vim, Emacs distributions)
- Container development environment support
- Git configuration with SSH signing
