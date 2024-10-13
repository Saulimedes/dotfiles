mcd() {
  if [ -z "$1" ]; then
    echo "Usage: mkcd <directory_name>"
    return 1
  fi

  mkdir -p "$1" && cd "$1"
}
