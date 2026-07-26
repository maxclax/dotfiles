---
name: commit-all
description: Use when a project has git submodules and all repos (root + submodules) need to be committed so nothing is left dirty
allowed-tools: Bash(git status, git diff, git add, git commit, git log, git submodule)
argument-hint: [message]
---

Commit all changes across the root project and every git submodule recursively. Nothing should be dirty when done.

## Commit message format (Doom Emacs convention)

Subject line: `type(scope1,scope2): summary` or `type: summary`

### Rules
- **Valid types**: `bump`, `dev`, `docs`, `feat`, `fix`, `merge`, `nit`, `perf`, `refactor`, `release`, `revert`, `test`, `tweak`
- **Subject length**: 10-72 characters (aim for ≤50)
- **Summary must NOT start with a capital letter**
- **Scopes**: comma-delimited, sorted alphabetically. Scopeless types: `bump`, `merge`, `release`, `revert`
- **Body lines** (if any): ≤72 characters (URLs exempt). Blank line between subject and body
- **Breaking changes**: use `!` after type/scope AND include `BREAKING CHANGE:` in body
- Commits starting with `fixup!`, `squash!`, or `WIP` skip validation

Reference: https://discourse.doomemacs.org/git-conventions

## Process

1. **Discover** — Run `git submodule foreach --recursive git status --short` to find all dirty submodules
2. **Also check root** — Run `git status --short` for the root repo
3. **Report** — Show a numbered list of all dirty repos (submodules + root) with their changes
4. **Commit inside-out** — Start from the deepest submodules first, work up to root:
   - For each dirty submodule, use `git -C <path>` instead of `cd` — never use `cd`:
     - `git -C <path> diff` to understand changes
     - `git -C <path> log --oneline -5` to match that repo's commit style
     - Stage relevant files by name (never `git add -A` or `git add .`)
     - Commit with a short 1-line message, no body
   - After all submodules are committed, commit the root repo (which will include updated submodule references)
5. **Verify** — Run `git submodule foreach --recursive git status --short` and `git status --short` to confirm everything is clean
6. If the user provides arguments, use that as the commit message for ALL repos: $ARGUMENTS

## Safety rules

- NEVER switch, create, or delete branches
- NEVER push to remote
- NEVER use `--force`, `--hard`, `--no-verify`, or `--amend`
- NEVER stage files that look like secrets (.env, credentials, tokens, keys)
- NEVER use `git reset`, `git rebase`, `git checkout`, or `git clean`
- If a repo has no changes, skip it
- Commit submodules BEFORE root so submodule refs are up to date
