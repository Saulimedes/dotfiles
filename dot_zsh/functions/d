# Open dired in current dir, cd to final dired directory on exit
d() {
  local tmpfile=$(mktemp /tmp/emacs-dired.XXXXXX)
  emacsclient -e "(setq my/dired-exit-file-pending \"$tmpfile\")" > /dev/null 2>&1
  emacsclient -t "$PWD"
  if [[ -s "$tmpfile" ]]; then
    cd -- "$(<"$tmpfile")"
  fi
  command rm -f "$tmpfile"
}
