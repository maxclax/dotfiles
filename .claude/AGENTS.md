# Agent Instructions

## Project Context

Personal dotfiles managed with **chezmoi** + **Nix Home Manager** on macOS.

- Source: `~/.local/share/chezmoi/`
- Applied to: `~/` via `chezmoi apply`
- Files ending in `.tmpl` use Go templating — never edit deployed files directly, always edit the source in chezmoi
- Encrypted files use age — add with `chezmoi add --encrypt <file>`
- Nix packages: `private_dot_config/home-manager-flake/packages.nix.tmpl`
- AI tools install script: `.chezmoiscripts/darwin/run_onchange_after_11-ai-tools.sh.tmpl`

## Safety Rules (always follow)

- Never run `chezmoi apply` without showing `chezmoi diff` first and getting confirmation
- Never commit without explicit user request
- Never use `git add -A` or `git add .` — stage specific files only
- Never skip hooks (`--no-verify`)
- Never force push
- Never edit files in `~/` directly — always edit the chezmoi source

## Template Conventions

When creating `.tmpl` files, always include the correct Emacs mode line on line 1:
- `*.el.tmpl` → `;;; filename.el -*- mode: emacs-lisp; lexical-binding: t; -*-`
- `*.sh.tmpl` → `# filename.sh -*- mode: sh; lexical-binding: t; -*-`
- `*.nix.tmpl` → `# -*- mode: nix; -*-`
- `*.yml.tmpl` / `*.yaml.tmpl` → `# -*- mode: yaml; -*-`
- `*.toml.tmpl` → `# -*- mode: conf-toml; -*-`

---

## Agents

### dotfiles — Modify any dotfile config

1. Ask what the user wants to change
2. Find the relevant file in `private_dot_config/` using glob/search
3. Read the current file to understand structure and conventions
4. Make the edit in the chezmoi source (never the deployed file)
5. Run `chezmoi diff` and show the output
6. Wait for confirmation, then run `chezmoi apply`

### add-nix-package — Add a package to Home Manager

1. Search nixpkgs to confirm the package name: `nix search nixpkgs <name>`
2. Open `private_dot_config/home-manager-flake/packages.nix.tmpl`
3. Find the appropriate category section (or create one)
4. Add the package with a short inline comment
5. Run `make hm_diff` and show output
6. Wait for confirmation, then run `make hm_update`
7. If `flake.lock` changed, run `make hm_commit`

### hm-update — Update Home Manager packages

1. Run `make hm_diff` and show output
2. Wait for confirmation
3. Run `make hm_update`
4. If `flake.lock` changed, ask user if they want to commit it with `make hm_commit`
5. If update fails, suggest `make hm_rollback`

### add-ai-tool — Add a new AI CLI tool

1. Check if it's available in nixpkgs: `nix search nixpkgs <name>`
2. If in nixpkgs and stable: add to `packages.nix.tmpl` under `# ── AI & LLM Tools`
3. If npm-based or needs latest version: add to `.chezmoiscripts/darwin/run_onchange_after_11-ai-tools.sh.tmpl` under the npm tools section
4. If curl installer: add a new section in the ai-tools script following the same pattern as Claude Code
5. Run `chezmoi diff`, confirm, then `chezmoi apply`
