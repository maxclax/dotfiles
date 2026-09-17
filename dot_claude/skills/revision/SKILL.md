---
name: revision
description: Use when the user wants the current project's TODO/backlog revised against reality ("revision", "actualize todo", "what is done / in progress / next") — finds the project backlog and any side-project briefs, checks every item against git, code and sessions, proposes status changes, and applies them only after confirmation
---

# Revision

Audit a project's task list against what actually exists, mark every item with
its real state, and do the same for side projects the project briefs (for
example a website that sells the app). Nothing is edited until the user
approves the proposed changes.

## 1. Locate

**Project.** Use the main checkout, not a worktree:

```bash
main=$(git worktree list --porcelain 2>/dev/null | awk '/^worktree /{print substr($0,10); exit}')
main=${main:-$(pwd)}; name=$(basename "$main")
key=$(printf '%s' "$name" | tr 'A-Z' 'a-z' | tr -cd 'a-z0-9')
```

**Backlog**, in this order — stop at the first hit:

1. A path named in the repo's `AGENTS.md` / `CLAUDE.md` (look for backlog, todo, org file).
2. A Denote file in `~/org` whose title matches the project:
   ```bash
   find -L ~/org -maxdepth 2 -name '*.org' | while read -r f; do
     t=$(basename "$f" | sed -E 's/^[0-9T]+--//; s/__.*//' | tr 'A-Z' 'a-z' | tr -cd 'a-z0-9')
     [ "$t" = "$key" ] && echo "$f"; done
   ```
3. A file in `~/org` that mentions the repo path: `grep -rlF "$main" ~/org --include='*.org'`.
4. In-repo lists: `TODO.org`, `ROADMAP.org`, `todo.md`, `docs/plans/*` with checkboxes.

Several candidates or none → ask the user which file is the backlog.

**Side projects.** A brief is a work order one repo writes for another; it can
live in either repo. Collect candidates:

```bash
find "$main" -maxdepth 3 -iname '*brief*' -not -path '*/node_modules/*' -not -path '*/.git/*'
grep -rliF "$name" ~/workspace --include='*brief*' 2>/dev/null | grep -v "^$main/"
```

plus `PROJECT` headings and Resources links to briefs or other `~/workspace`
repos in the backlog. Read each candidate's title and first section, then sort:

- **outgoing** — the brief is about this project and another repo builds it:
  that repo is a side project to revise;
- **incoming** — this repo builds it: its tasks are part of this project's work;
- a brief that only mentions the project is neither — drop it.

For each side project note: the brief file, the side repo, and that repo's own
backlog (found the same way). Show the user the map before researching:

```
Project  my-app   (~/workspace/…)     backlog ~/org/…--my-app__….org
Side     site.example                  brief ~/workspace/site.example/my-app-brief.org
                                       backlog ~/org/…--siteexample__….org
```

## 2. Research

Read the backlog and briefs in full. For each open item (`TODO`, `NEXT`,
`INPROCESS`, `WAITING`, `HOLD`, `PROJECT`, unticked checkboxes) and each
`DONE` item closed in the last 30 days, look for evidence:

- `git log --all --since=<item date or 60 days> --oneline` and
  `git log --all -S'<distinctive term>'`; branches and worktrees named after it;
- the code itself: does the feature, page, file or setting exist and work;
- recent Claude sessions of the repo
  (`~/.claude/projects/<path with / and . as ->/*.jsonl`) for work not yet committed;
- the item's own notes, CLOSED/LOGBOOK lines, dated corrections and answers.

Classify each item:

| Verdict | Meaning |
|---|---|
| done | shipped; evidence found |
| in progress | partial commits, open branch/worktree, or started in a session |
| next | not started, unblocked, highest priority or depends only on done items |
| blocked | waits on a person, decision or another item — name it |
| stale | superseded, duplicated or no longer relevant — say by what |
| not really done | marked `DONE` but the evidence is missing or broken |
| unclear | cannot tell — say what would settle it |

Group duplicates and related items first; a follow-up can change the verdict.

**Side projects** — for each brief, compare it with the side repo:

- which brief tasks are done, in progress or not started there;
- where the side repo drifted from the brief (built differently, missing parts);
- what the brief is missing because the main project changed since the brief's
  last dated section (new features, renamed things, removed flows);
- improvements worth handing over, each as a concrete task.

## 3. Propose

Report before editing:

```markdown
## Revision — <project> (<date>)

### Project backlog (<file>)
| # | Item | Now | Proposed | Evidence |
|---|---|---|---|---|

### Incoming brief from <project> (<brief file>)
| # | Brief task | Now | Proposed | Evidence |
|---|---|---|---|---|

### Side project: <name> (<brief file>)
| # | Brief task | Now | Proposed | Evidence |
|---|---|---|---|---|
- **Drift:** …
- **Brief needs:** … (numbered, ready to paste as tasks)

### Next up
1. … — why first
```

Then ask with AskUserQuestion (header "Apply"): apply all / choose items / report only.

## 4. Apply (only what was approved)

Before writing an org file, check it is not being edited in Emacs: an Emacs
lock file `.#<filename>` in the same directory means unsaved edits — stop and
ask the user to save first.

Write states exactly as the user's config does — sequences
`TODO NEXT INPROCESS | DONE`, `PROJECT | DONE CANCELLED`,
`WAITING DELEGATED HOLD | CANCELLED SKIP`:

- **done** → `DONE`, add the `claude` tag, and a `CLOSED:` line under the heading:
  `CLOSED: [2026-09-17 Thu 14:03]` (real local time).
- **in progress** → `INPROCESS`, add the `claude` tag.
- **next** → `NEXT`.
- **blocked** → `WAITING` (or `HOLD`), with one line saying on what.
- **stale** → `CANCELLED` with one line saying why; never delete an item.
- **not really done** → back to `TODO` with one line of what is missing.
- Checkboxes → `- [X] … ✅ [2026-09-17 Thu 14:03]`; Markdown → `- [x] … (claude, closed 2026-09-17 14:03)`.

Never rewrite the user's task text, priorities or other tags; add only the
state, tag, stamp and one note line. Leave headings in place.

Add one entry under the backlog's logging heading (`🗒️ Loging` or similar),
in its existing format:
`** [2026-09-17 Thu 14:03] revision (claude): 4 done, 2 in progress, 1 next, 1 cancelled`

For each side project with approved brief changes, append a dated section to
the brief (not the side repo's code), in the brief's own style:
`** TODO <n>. Revision <date> — what changed on the <project> side` with the
drift and the new tasks. Mirror a one-line `PROJECT` note in the side
project's backlog if it has one.

## 5. Finish

Report what was written, file by file, and the numbered "Next up" list so the
user can pick one. Do not commit repo files; the org files are outside git.
