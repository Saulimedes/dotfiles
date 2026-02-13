_ai_system_prompt="You are a zsh command generator on Gentoo Linux. Rules:
- Output ONLY the command. No explanation, no markdown, no code fences, no comments.
- If the input is a natural language prompt, generate the appropriate command.
- If the input is a command with errors (typos, wrong flags, bad syntax), output the corrected command.
- If multiple commands are needed, chain them with && or pipes.
- Prefer common tools: eza over ls, bat over cat, rg over grep, fd over find.
- Respond with nothing but the command."

_ai_run() {
  opencode run --format json "$_ai_system_prompt" "$1" 2>/dev/null \
    | jq -r 'select(.type == "text") | .part.text' \
    | tr -d '\n'
}

ai() {
  local prompt="$*"
  if [ -z "$prompt" ]; then
    echo "Usage: ai <prompt>"
    return 1
  fi

  local result
  result=$(_ai_run "$prompt")

  if [ -z "$result" ]; then
    echo "No response from opencode"
    return 1
  fi

  print -z "$result"
}

# ZLE widget: Ctrl+O toggles between original prompt and AI suggestion
_ai_original=""
_ai_suggestion=""

_ai_set_buffer() {
  BUFFER=""
  CURSOR=0
  zle -R
  LBUFFER="$1"
  RBUFFER=""
  zle reset-prompt
}

ai-command-widget() {
  # Toggle back to original if we're showing the suggestion
  if [[ -n "$_ai_suggestion" && "$BUFFER" == "$_ai_suggestion" ]]; then
    _ai_set_buffer "$_ai_original"
    return
  fi

  # Toggle back to suggestion if we're showing the original and have a cached result
  if [[ -n "$_ai_suggestion" && "$BUFFER" == "$_ai_original" ]]; then
    _ai_set_buffer "$_ai_suggestion"
    return
  fi

  # New prompt — fetch from opencode
  if [ -z "$BUFFER" ]; then
    return
  fi

  _ai_original="$BUFFER"
  _ai_suggestion=""
  _ai_set_buffer "  asking opencode..."

  local result
  result=$(_ai_run "$_ai_original")

  if [ -n "$result" ]; then
    _ai_suggestion="$result"
    _ai_set_buffer "$result"
  else
    _ai_set_buffer "$_ai_original"
  fi
}

zle -N ai-command-widget
bindkey '^O' ai-command-widget
