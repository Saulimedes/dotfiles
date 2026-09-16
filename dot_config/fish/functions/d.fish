# Open dired in current dir; cd to the final dired directory on exit
function d
    set -l tmpfile (mktemp /tmp/emacs-dired.XXXXXX)
    emacsclient -e "(setq my/dired-exit-file-pending \"$tmpfile\")" >/dev/null 2>&1
    emacsclient -t $PWD
    if test -s $tmpfile
        cd -- (cat $tmpfile)
    end
    command rm -f $tmpfile
end
