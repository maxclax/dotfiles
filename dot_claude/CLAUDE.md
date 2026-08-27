# Claude rules

## Working on TODOs in a project's .org / .md files

When asked to work on a task in a project's `.org` or `.md` TODO file:

1. **Gather the full picture before acting.** Collect every related or similar
   task in that file and in sibling task files in the project. Duplicates,
   blockers and follow-ups change what the correct change actually is — do not
   work off the single line I pointed at.
2. **Claim the task.** When starting, set it to `INPROCESS` and tag it
   `claude`, so both the state and the fact Claude touched it are visible.
   `INPROCESS` exists in the `TODO → NEXT → INPROCESS → | DONE` sequence; items
   in the `PROJECT` or `WAITING` sequences have no such state, so for those just
   add the tag.
3. **Close it properly.** When the work is done, mark it done, keep the `claude`
   tag, and record the closing time.

### Stamp formats

I edit files directly, so the Emacs hooks that normally write these do not run —
write them by hand, in exactly the formats the config already produces:

- **Org heading** — `INPROCESS` while working, `DONE` when finished. Keep
  `:claude:` in the tag list and add the `CLOSED:` line under the headline:

  ```org
  ** INPROCESS Some task                                         :existing:claude:

  ** DONE Some task                                              :existing:claude:
     CLOSED: [2026-08-27 Thu 14:03]
  ```

- **Org checkbox** — tick it and append the stamp at end of line
  (`my/org-checkbox-stamp-format`):

  ```org
  - [X] Some task ✅ [2026-08-27 Thu 14:03]
  ```

- **Markdown** — `- [-]` while working, `- [x]` when finished, with the tag and
  time appended on completion:

  ```markdown
  - [-] Some task (claude, in progress)
  - [x] Some task (claude, closed 2026-08-27 14:03)
  ```

Use the real current date and time, never a placeholder. Do not invent other
timestamp formats — these stay greppable and match what Emacs writes.
