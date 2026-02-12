---
name: commit
description: Create a git commit without Co-Authored-By sign-off
allowed-tools: Bash(git status, git diff, git add, git commit, git log)
argument-hint: [message]
---

Create a git commit for the current changes. Do NOT include "Co-Authored-By" or any sign-off lines.

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
- Use prefix conventions from recent commits (e.g. `feat:`, `fix:`, `nit:`, `dev:`)
