---
name: promote
description: Use when work done on a feature/machine branch should land on the trunk first — commits the current uncommitted changes in the main worktree (main or master), pushes it, then merges trunk back into the working branch
allowed-tools: Bash(git status, git diff, git apply, git add, git commit, git push, git merge, git log, git worktree, git show, git rev-list, git checkout, git cat-file, cmp, cp, rm, rg)
argument-hint: [message]
---

Land the current uncommitted changes on the trunk instead of the working branch:
commit them in the **trunk worktree**, push it, then merge trunk back into the
working branch so both carry the change.

Use this when a repo keeps the trunk (`main`/`master`) in a separate worktree and
day-to-day work happens on another branch (`personal`, `develop`, …) that must not
be the origin of shared commits.

Commit messages follow the same rules as the `commit` skill: Doom convention,
`type: lowercase summary` (10–72 chars, aim ≤50), no `Co-Authored-By` or any
sign-off. Split the work into logical commits, staging files by name.

## Steps

### 1. Discover the layout — never assume names

```bash
git worktree list --porcelain          # paths + branches
git branch --show-current              # the working branch
```

- Trunk = whichever worktree is on `refs/heads/main` or `refs/heads/master`.
- Working branch = the branch of the worktree you are in.
- If no separate trunk worktree exists, **stop and ask**. Do not switch, create,
  or delete branches to work around it.
- If you are already on the trunk, this skill does not apply — use `commit`.

### 2. Preflight

```bash
git status --porcelain                 # working worktree: what to move
git -C <trunk> status --porcelain      # MUST be empty
```

Abort if the trunk worktree is dirty. If the working worktree is clean, there is
nothing to move — skip to step 6 (push) and 7 (merge).

### 3. Privacy audit — before anything is committed

Scan only the **added** lines plus any new files for: project/client/account
names, emails, API-key shapes (`sk-`, `ghp_`, `Bearer `), `op://` references,
absolute machine paths, non-loopback IPs. Report what you checked. Stop and ask
if anything matches.

### 4. Move the changes to the trunk worktree

```bash
git diff HEAD > /tmp/promote.patch                  # tracked, incl. staged renames
git -C <trunk> apply --3way /tmp/promote.patch      # 3way survives divergence
cp <untracked files> <trunk>/<same relative path>   # the `??` entries
```

Then prove nothing was lost: `cmp` each transferred file against the working
copy. Files that legitimately differ (trunk lacks branch-only content) must
differ **only** in those pre-existing regions — check with
`git diff <trunk-branch> <working-branch> -- <file>` before continuing.

### 5. Commit in the trunk worktree

```bash
git -C <trunk> reset                                # stage per commit, by name
git -C <trunk> add <files for this commit>
git -C <trunk> commit -m "type: summary"            # do NOT pipe this
git -C <trunk> log --oneline -1                     # confirm the hash moved
```

Never `git add -A`/`.`; never stage anything secret-shaped.

**Never pipe `git commit` into another command.** A pipeline's exit status is the
last command's, so a hook rejection returns 0 and `set -e` will not stop the run.
The staged files then ride along in the *next* commit, producing a mixed commit
with a wrong message — unfixable once pushed. Run the commit bare, and confirm
`log --oneline -1` shows the new subject before staging the next group.

The summary is checked by a commit-msg hook: **lowercase after the type**, even
for identifiers. `feat: H toggles dotfiles` is rejected; write
`feat: toggle dotfiles with H` instead.

### 6. Push the trunk

```bash
git -C <trunk> push origin <trunk-branch>
```

No `--force`, no `--no-verify`. Report the ref update.

### 7. Merge trunk into the working branch

Verify the trunk really has the work **before** discarding the working copies —
this is the only destructive step:

```bash
git cat-file -e <trunk-branch>:<path>          # new files exist
git show <trunk-branch>:<path> | rg <marker>   # edits are present
```

Then, in the working worktree:

```bash
git checkout -- <the files that were moved>    # discard now-redundant copies
rm <untracked files that were copied>          # the merge brings them back
git merge <trunk-branch>
```

Confirm afterwards that branch-only content survived the merge (private entries,
machine-specific settings) and that the moved change is present.

### 8. Report

- Trunk commits with hashes, and the push ref update
- The merge commit
- What the privacy audit covered
- That the **working branch is left unpushed** — say so, and offer. Only push it
  when asked.

## Safety rules

- NEVER switch, create, or delete branches.
- NEVER `--force`, `--hard`, `--no-verify`, `--amend`, `git rebase`, `git clean`.
- NEVER discard working-worktree changes until the trunk provably has them.
- NEVER stage `.env*`, credentials, tokens, keys.
- If a step fails (patch conflict, rejected push, merge conflict), stop and
  report — do not improvise around it.
