if [ -n "$PROJECT_KUBECONFIG" ] && [ -d "$PROJECT_KUBECONFIG" ]; then
  export KUBECONFIG=$(printf '%s:' "$PROJECT_KUBECONFIG"/*.yaml 2>/dev/null | sed 's/:$//')
fi
