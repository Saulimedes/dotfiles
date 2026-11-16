# ~/.local/bin/kns: namespaces of the current context
complete -c kns -f -a '(kubectl get ns -o name 2>/dev/null | string replace namespace/ "")'
