killport() {
  if [[ -z "$1" ]]; then
    echo "Usage: killport <port>"
    return 1
  fi
  local pids=$(fuser "$1/tcp" "$1/udp" 2>/dev/null)
  if [[ -n "$pids" ]]; then
    echo "$pids" | xargs kill -9
    echo "Killed process on port $1"
  else
    echo "No process found on port $1"
  fi
}
