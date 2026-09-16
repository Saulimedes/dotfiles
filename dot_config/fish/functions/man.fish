function man
    emacsclient -t --eval "(man \"$argv\")" 2>/dev/null
    or command man $argv
end
