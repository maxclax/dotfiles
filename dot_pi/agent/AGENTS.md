# AGENTS.md

Global instructions for the pi coding agent. A repo's own AGENTS.md takes
precedence over this file; keep this to preferences that hold everywhere.

## Editor

- Emacs is Doom **without evil mode**. Never suggest `SPC`-prefixed or vim
  keybindings; use standard `C-x` / `C-c` bindings.
- Dvorak on an ergonomic keyboard — prefer bindings that are comfortable there
  over ones that are merely mnemonic in QWERTY.

## Git

- Do not commit unless asked. Batch changes so history can be squashed first.
- Never merge or push branches on your own initiative.
- Commit subjects follow the Doom convention: `type: lowercase summary`, under
  72 characters, no trailing period. Valid types include feat, fix, tweak, nit,
  dev, docs, refactor, perf, test, revert, bump, merge, release.

## Comments and examples

- Never put project, client, or account names in comments, examples, or commit
  messages. Use neutral placeholders.
- Prefer few comments that explain *why*. Skip machine-specific measurements,
  dates and paths — they go stale and leak detail.

## Verification

- Prefer reading the installed source over recalling an API. Version-specific
  details (keybindings, flags, defaults) must be checked, not guessed.
- Say plainly when something is unverified rather than presenting it as fact.

## Context

- The local models run with a limited context window. Read the specific region
  of a file you need instead of whole large documents, and prefer `rg` over
  dumping directories.
