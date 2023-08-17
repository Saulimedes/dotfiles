function __git_branches
    git for-each-ref --format='%(refname:short)' refs/heads
end

function __git_complete_branches
    set -l branches (__git_branches)
    for branch in $branches
        echo $branch
    end
end

complete -f -c git-brancher -a '(__git_complete_branches)'
