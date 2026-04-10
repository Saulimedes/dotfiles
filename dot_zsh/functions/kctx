kctx() {
  if ! command -v kubectl &>/dev/null; then
    echo "kubectl not found" >&2
    return 1
  fi
  kubectl config use-context "${1:-$(kubectl config get-contexts -o name | fzf)}"
}
