function mcd
    if test -z "$argv[1]"
        echo "Usage: mcd <directory>"
        return 1
    end
    mkdir -p $argv[1] && cd $argv[1]
end
