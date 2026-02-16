#!/bin/sh
if [ -n "$PROJECT_KUBECONFIG" ] && [ -d "$PROJECT_KUBECONFIG" ]; then
  KUBECONFIG=$(printf '%s:' "$PROJECT_KUBECONFIG"/*.yaml 2>/dev/null | sed 's/:$//')
  export KUBECONFIG
fi
