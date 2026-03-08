# Custom Agent Instructions

## dotfiles
When user wants to modify dotfiles configuration:
1. Ask what they want to change
2. Find the relevant file in private_dot_config/ using glob/search
3. Read the current config to understand structure
4. Make the edit
5. Run `chezmoi diff` to preview changes
6. Ask user to confirm before running `chezmoi apply`

## hm-update
Update Home Manager packages:
1. Run `make hm_diff` to preview changes
2. Show the user the diff output
3. Ask for confirmation
4. If confirmed, run `make hm_update`
5. Run `make hm_commit` if flake.lock changed

## chezmoi-apply
Apply chezmoi changes:
1. Run `chezmoi diff` to preview
2. If user confirms, run `chezmoi apply`

## backup
Create a Borg backup:
1. Run `make backup_create`
2. Show the output

## update-apps
Update all apps on macOS:
1. Run `make update_apps`
