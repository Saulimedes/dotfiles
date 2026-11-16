# ~/.local/bin/kctx: kubectl context names
complete -c kctx -f -a '(kubectl config get-contexts -o name 2>/dev/null)'
