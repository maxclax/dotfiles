---
name: anything-else
description: Use when a conversation has covered multiple topics or tasks and the user wants to verify nothing was missed — scans recent conversation history for incomplete, skipped, or forgotten items
---

# Anything Else

## Overview

Review recent conversation to find tasks, requests, list items, or intentions that were mentioned but never completed. Catches things that got lost when the conversation moved to a new topic.

## When to Use

- User asks "did I miss anything?" or "anything else?"
- User invokes `/anything-else`
- A conversation has covered many topics and you suspect items were dropped

## Process

1. **Scan** — Review the last ~1-2 hours of conversation (not the entire history)
2. **Extract** — Identify every item that looks like a task, request, or intention:
   - Explicit requests ("can you also...", "and then...")
   - List items the user wrote (numbered or bulleted)
   - Items the user said they wanted to do next
   - Plans or intentions mentioned in passing
   - Sub-tasks within a larger request
3. **Check** — For each item, determine if it was:
   - **Completed** — action was taken and confirmed
   - **Partially done** — started but not finished
   - **Skipped** — never addressed
   - **Superseded** — replaced by a different approach
4. **Report** — Present findings clearly

## Report Format

```markdown
## Conversation Review (last ~N hours)

### Completed
- [item] — done in [context]

### Still Open
1. [item] — mentioned when [context], not yet addressed
2. [item] — partially done: [what's left]
3. [item] — ...

### Superseded / No Longer Relevant
- [item] — replaced by [what happened instead]

**Which number do you want to tackle next?** (or "all", or "none")
```

Only include sections that have items. If everything was completed, say so.

**Still Open items MUST be numbered** so the user can reply with a number to pick what to do next. After the user replies with a number, immediately start working on that item. If the user says "all", work through them in order.

## Common Patterns to Watch For

- User writes a numbered list, conversation completes items 1-3 but skips item 4
- User says "also..." or "and..." mid-conversation — the second request gets lost
- User mentions a follow-up task ("after that we should...") that never happens
- A task is started but interrupted by a new topic
- User asks multiple questions in one message — only some get answered
