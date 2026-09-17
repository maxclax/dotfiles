---
name: recap
description: Use when the user asks what was done or changed recently ("recap", "summarize work", "what did we do in the last hour") — asks for a time window (since last recap, 30m, 1h, 3h), then reports work done and changes made across all sessions of the current project plus git
---

# Recap

Summarize what was done and what changed in a time window, from every Claude
session of the current project (not just this conversation) and from git.

## 1. Pick the window

If the user already gave one (`/recap 2h`, "last 30 minutes"), use it and skip
the question. Otherwise read the last recap time for this project:

```bash
cat ~/.claude/state/recap/"$(pwd | tr '/.' '--')" 2>/dev/null
```

Ask with AskUserQuestion (header "Window"), options in this order:

- **Since last recap (HH:MM)** — only if the state file exists; show its local time
- **Last 30 minutes**
- **Last 1 hour**
- **Last 3 hours**

## 2. Collect

Run the extractor with the window start (minutes back, or the ISO timestamp
from the state file). It lists every prompt and action per session, oldest
first, in local time.

```bash
python3 - "<MINUTES or ISO>" "$(pwd)" <<'PY'
import datetime as dt, glob, json, os, sys
arg, cwd = sys.argv[1], sys.argv[2]
now = dt.datetime.now(dt.timezone.utc)
since = now - dt.timedelta(minutes=int(arg)) if arg.isdigit() else dt.datetime.fromisoformat(arg.replace("Z", "+00:00"))
cut = since.strftime("%Y-%m-%dT%H:%M:%S")
pdir = os.path.expanduser("~/.claude/projects/" + cwd.replace("/", "-").replace(".", "-"))
print(f"window {since.astimezone():%Y-%m-%d %H:%M} -> {now.astimezone():%H:%M} local")
for f in sorted(glob.glob(pdir + "/*.jsonl"), key=os.path.getmtime):
    if os.path.getmtime(f) < since.timestamp():
        continue
    rows = []
    for line in open(f, encoding="utf-8", errors="replace"):
        try:
            o = json.loads(line)
        except ValueError:
            continue
        ts = o.get("timestamp", "")
        if ts[:19] < cut or o.get("isSidechain"):
            continue
        c = (o.get("message") or {}).get("content")
        t = f"{dt.datetime.fromisoformat(ts[:19] + '+00:00').astimezone():%H:%M}"
        if o.get("type") == "user" and isinstance(c, str) and not c.startswith("<"):
            rows.append(f"{t} USER  {c.strip()[:300]}")
        elif o.get("type") == "assistant" and isinstance(c, list):
            for b in c:
                if b.get("type") != "tool_use":
                    continue
                i = b.get("input", {})
                what = i.get("file_path") or i.get("description") or i.get("command", "")[:120] or i.get("skill", "")
                rows.append(f"{t}   {b['name']}: {what}")
    if rows:
        print(f"\n=== session {os.path.basename(f)[:8]} ({len(rows)} entries)")
        print("\n".join(rows))
PY
```

Then git, for the repo and every worktree of it (skip if not a repo):

```bash
git log --all --since="<window start ISO>" --date=format-local:%H:%M \
  --format='%h %ad %D %s' --name-status
git worktree list
git for-each-ref --format='%(refname:short) %(upstream:track)' refs/heads
git status --short   # in each worktree
```

Read the extractor output and git yourself; do not pipe it through more
scripts. If the window is long and the output is large, read it in parts.

## 3. Report

```markdown
## Recap — HH:MM to HH:MM (<window>)

### Done
- **<topic>** — what was asked, what was done, outcome (verified / failed / left)

### Changes
| Commit | Branch | Summary | Pushed |
|---|---|---|---|

- **Uncommitted:** file — one-line what
- **Outside the repo:** deployed files, installs, settings, external services

### Still open
1. <item> — why it is open
```

Rules:

- Group by topic, not by session or time; one line of outcome per topic.
- Say what actually happened: failed or abandoned work is reported as such.
- Changes made by other sessions count — mark them "(other session)".
- Keep private details out if the recap may be shared: no secrets, keys,
  passwords or tokens, even if they appear in the transcript.
- Omit empty sections. Number the open items so the user can pick one.

## 4. Remember

After reporting, store the end of the window for "since last recap":

```bash
mkdir -p ~/.claude/state/recap && date -u +%Y-%m-%dT%H:%M:%SZ > ~/.claude/state/recap/"$(pwd | tr '/.' '--')"
```
