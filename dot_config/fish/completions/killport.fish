# ~/.local/bin/killport: ports something is listening on right now
complete -c killport -f -a '(ss -tulnH 2>/dev/null | awk \'{n = split($5, a, ":"); print a[n]}\' | sort -un)'
