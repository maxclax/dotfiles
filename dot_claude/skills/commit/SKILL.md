---
name: commit
description: Create a git commit without Co-Authored-By sign-off
allowed-tools: Bash(git status, git diff, git add, git commit, git log)
argument-hint: [message]
---

Create a git commit for the current changes. Do NOT include "Co-Authored-By" or any sign-off lines.

## Commit message format (Doom Emacs convention)

Subject line: `type(scope1,scope2): summary` or `type: summary`

### Rules
- **Valid types**: `bump`, `dev`, `docs`, `feat`, `fix`, `merge`, `nit`, `perf`, `refactor`, `release`, `revert`, `test`, `tweak`
- **Subject length**: 10-72 characters (aim for ≤50)
- **Summary must NOT start with a capital letter**
- **Scopes**: comma-delimited, sorted alphabetically. Scopeless types: `bump`, `merge`, `release`, `revert`
- **Body lines** (if any): ≤72 characters (URLs exempt). Blank line between subject and body
- **Breaking changes**: use `!` after type/scope AND include `BREAKING CHANGE:` in body
- **Trailers**: `Fix:`, `Ref:`, `Close:`, `Co-authored-by:`, `Signed-off-by:` — names need `Name <email>` format, hashes must be 12 chars
- Commits starting with `fixup!`, `squash!`, or `WIP` skip validation

Reference: https://discourse.doomemacs.org/git-conventions

## Steps

1. Run `git status` and `git diff` to understand the changes
2. Run `git log --oneline -5` to match commit message style
3. Stage only the relevant files by name (never use `git add -A` or `git add .`)
4. Commit with a short 1-line message, no body
5. If the user provides arguments, use them as the commit message: $ARGUMENTS

## Safety rules

- NEVER switch, create, or delete branches
- NEVER push to remote
- NEVER use `--force`, `--hard`, `--no-verify`, or `--amend`
- NEVER stage files that look like secrets (.env, credentials, tokens, keys)
- NEVER use `git reset`, `git rebase`, `git checkout`, or `git clean`
- If there are no changes to commit, say so and stop
