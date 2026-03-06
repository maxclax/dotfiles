#!/usr/bin/env bash
# Claude Code PreToolUse hook — blocks git commits with Claude watermarks in the message.

input=$(cat)
command=$(echo "$input" | jq -r '.tool_input.command // ""')

if ! echo "$command" | grep -qE "git\b.*\bcommit\b"; then
  exit 0
fi

if echo "$command" | grep -qiE "Co-Authored-By:.*[Cc]laude|Co-Authored-By:.*[Aa]nthropic|noreply@anthropic\.com|Generated with \[Claude|🤖 Generated|claude\.ai/code"; then
  echo "ERROR: Claude watermark detected in commit command. Remove it before committing." >&2
  exit 2
fi

exit 0
