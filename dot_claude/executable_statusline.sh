#!/usr/bin/env bash
# Claude Code status line: model (context|cost|5h limit|week limit) … project (branch) right-aligned
export LC_CTYPE=en_US.UTF-8
input=$(cat)

j() { echo "$input" | jq -r "$1 // empty" 2>/dev/null; }

model=$(j '.model.display_name')
ctx=$(j '.context_window.used_percentage')
[ -z "$ctx" ] && {
  used=$(j '.context_window.total_input_tokens'); size=$(j '.context_window.context_window_size')
  [ -n "$used" ] && [ -n "$size" ] && [ "$size" -gt 0 ] && ctx=$((used * 100 / size))
}
cost=$(j '.cost.total_cost_usd')
five=$(j '.rate_limits.five_hour.used_percentage')
five_at=$(j '.rate_limits.five_hour.resets_at')
week=$(j '.rate_limits.seven_day.used_percentage')
week_at=$(j '.rate_limits.seven_day.resets_at')
dir=$(j '.workspace.current_dir')
branch=$(git -C "${dir:-.}" branch --show-current 2>/dev/null)

# epoch → local time; macOS date -r, GNU date -d
at() { date -r "$1" +"$2" 2>/dev/null || date -d "@$1" +"$2" 2>/dev/null; }

parts=()
[ -n "$ctx" ]  && parts+=("ctx${ctx%.*}%")
[ -n "$cost" ] && parts+=("\$$(printf '%.0f' "$cost")")
[ -n "$five" ] && parts+=("5h${five%.*}%${five_at:+ ↻$(at "$five_at" %H:%M)}")
[ -n "$week" ] && parts+=("wk${week%.*}%${week_at:+ ↻$(at "$week_at" %a)}")
left="${model:-?}"
[ ${#parts[@]} -gt 0 ] && left="$left ($(IFS='|'; echo "${parts[*]}"))"

right="${dir/#"$HOME"/"~"}${branch:+ ($branch)}"

gap=$(( ${COLUMNS:-0} - ${#left} - ${#right} - 1 ))
[ "$gap" -lt 2 ] && gap=2
printf '%s%*s%s' "$left" "$gap" '' "$right"
