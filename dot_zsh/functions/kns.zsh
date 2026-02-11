kns() {
  if ! command -v kubectl &>/dev/null; then
    echo "kubectl not found" >&2
    return 1
  fi
  kubectl config set-context --current --namespace "${1:-$(kubectl get ns -o jsonpath='{.items[*].metadata.name}' | tr ' ' '\n' | fzf)}"
}
