---
name: commit
description: Create a git commit without Co-Authored-By sign-off
allowed-tools: Bash(git status, git diff, git add, git commit, git log, rg)
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

## Privacy check (blocking — run after staging, before committing)

This repo is public-facing. Nothing in it may identify a private project,
client, employer, or their infrastructure — not in code, not in comments, not
in docs, and not in the commit message.

Review the staged diff and reject anything that names something real and
private:

- **Project / client / company names**, including in file names and paths
- **Domains and hostnames** belonging to those projects (prod, staging, or
  internal), and any URL containing them
- **Server IPs**, internal network ranges, cluster or namespace names
- **Personal or work email addresses** other than the committer's own
- **Absolute paths that expose a client**, e.g. `~/workspace/<clientname>/...`
- **Ticket / issue IDs** from a private tracker

Allowed, because they identify public things rather than private ones:

- Names of libraries, tools, languages, distros, and their upstream URLs and
  docs links — a real reference is the point (`nixpkgs`, `poppler`, `mu4e`,
  `https://espanso.org/docs/`)
- Public registries and services (`github.com`, `crates.io`, `pypi.org`)
- Generic placeholders: `example.com`, `myproject`, `<client>`, `api.internal`

**Rewrite, don't just delete.** Describe the *role*, not the identity: "a
client's staging cluster", "the main work project", "a Django app deployed to
k3s". The change stays understandable; the name goes away.

To surface candidates in the staged diff (then judge each hit — most are
fine):

```bash
git diff --cached | rg "^\+" | rg -i "[a-z0-9-]+\.(com|net|org|io|dev|app|cloud|internal|local)|([0-9]{1,3}\.){3}[0-9]{1,3}|[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+|/Users/[^/ ]+/"
```

If anything private is found: fix the content, re-stage, re-check. Do not
commit until the check is clean. If unsure whether a name is private, ask
rather than commit it.

## Steps

1. Run `git status` and `git diff` to understand the changes
2. Run `git log --oneline -5` to match commit message style
3. Stage only the relevant files by name (never use `git add -A` or `git add .`)
4. Run the **Privacy check** above on the staged diff, and on the message you
   are about to write
5. Commit with a short 1-line message, no body
6. If the user provides arguments, use them as the commit message: $ARGUMENTS

## Safety rules

- NEVER switch, create, or delete branches
- NEVER push to remote
- NEVER use `--force`, `--hard`, `--no-verify`, or `--amend`
- NEVER stage files that look like secrets (.env, credentials, tokens, keys)
- NEVER commit content that fails the Privacy check above
- NEVER use `git reset`, `git rebase`, `git checkout`, or `git clean`
- If there are no changes to commit, say so and stop
