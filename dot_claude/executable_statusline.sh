#!/usr/bin/env bash
# Claude Code status line: model · context used · cost · session/week limits · dir · branch
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
week=$(j '.rate_limits.seven_day.used_percentage')
dir=$(j '.workspace.current_dir')
branch=$(git -C "${dir:-.}" branch --show-current 2>/dev/null)

out="${model:-?}"
[ -n "$ctx" ]  && out="$out · ctx ${ctx%.*}%"
[ -n "$cost" ] && out="$out · \$$(printf '%.2f' "$cost")"
[ -n "$five" ] && out="$out · 5h ${five%.*}%"
[ -n "$week" ] && out="$out · wk ${week%.*}%"
[ -n "$dir" ]  && out="$out · ${dir/#"$HOME"/"~"}"
[ -n "$branch" ] && out="$out ($branch)"
printf '%s' "$out"
