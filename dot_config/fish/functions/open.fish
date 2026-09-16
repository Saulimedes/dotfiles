function _open_remote_url
    set -l url (git remote get-url origin 2>/dev/null); or return 1
    set url (string replace 'git@' '' $url)
    set url (string replace -r '\.git$' '' $url)
    set url (string replace ':' '/' $url)
    if not string match -q 'https://*' $url
        set url "https://$url"
    end
    echo $url
end

function _open_forge_type
    set -l url (git remote get-url origin 2>/dev/null); or return
    if string match -q '*gitlab*' $url
        echo gitlab
    else if string match -q '*codeberg*' $url
        or string match -q '*gitea*' $url
        or string match -q '*forgejo*' $url
        echo gitea
    else
        echo github
    end
end

function _open_branch
    git symbolic-ref --short HEAD 2>/dev/null
end

function _open_default_branch
    set -l branch (git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null \
        | string replace 'origin/' '')
    if test -z "$branch"
        if git show-ref --verify --quiet refs/heads/main 2>/dev/null
            set branch main
        else if git show-ref --verify --quiet refs/heads/master 2>/dev/null
            set branch master
        end
    end
    if test -n "$branch"
        echo $branch
    else
        echo main
    end
end

function open
    switch $argv[1]
        case ''
            set -l url (_open_remote_url)
            if test $status -ne 0
                xdg-open . &>/dev/null &
                return
            end
            set -l forge (_open_forge_type)
            set -l branch (_open_branch)
            set -l default (_open_default_branch)
            if test -n "$branch" -a "$branch" != "$default"
                switch $forge
                    case gitlab;  set url "$url/-/tree/$branch"
                    case gitea;   set url "$url/src/branch/$branch"
                    case '*';     set url "$url/tree/$branch"
                end
            end
            xdg-open $url &>/dev/null &

        case pr
            set -l url (_open_remote_url); or begin; echo "Not a git repo"; return 1; end
            set -l forge (_open_forge_type)
            set -l branch (_open_branch)
            set -l default (_open_default_branch)
            if test "$branch" = "$default"
                switch $forge
                    case gitlab;  set url "$url/-/merge_requests"
                    case '*';     set url "$url/pulls"
                end
            else
                switch $forge
                    case gitlab;  set url "$url/-/merge_requests/new?merge_request[source_branch]=$branch"
                    case github;  set url "$url/compare/$default...$branch?expand=1"
                    case gitea;   set url "$url/compare/$default...$branch"
                end
            end
            xdg-open $url &>/dev/null &

        case issues
            set -l url (_open_remote_url); or begin; echo "Not a git repo"; return 1; end
            switch (_open_forge_type)
                case gitlab;  set url "$url/-/issues"
                case '*';     set url "$url/issues"
            end
            xdg-open $url &>/dev/null &

        case 'actions' 'ci'
            set -l url (_open_remote_url); or begin; echo "Not a git repo"; return 1; end
            switch (_open_forge_type)
                case gitlab;  set url "$url/-/pipelines"
                case '*';     set url "$url/actions"
            end
            xdg-open $url &>/dev/null &

        case '*'
            # file:line pattern
            if string match -qr '^(.+):([0-9]+)$' -- $argv[1]
                set -l file (string replace -r ':([0-9]+)$' '' $argv[1])
                set -l line (string match -r '[0-9]+$' $argv[1])
                if test -f $file
                    set -l url (_open_remote_url); or begin; echo "Not a git repo"; return 1; end
                    set -l forge (_open_forge_type)
                    set -l branch (_open_branch)
                    set -l relpath (realpath --relative-to=(git rev-parse --show-toplevel) $file)
                    switch $forge
                        case gitlab;  set url "$url/-/blob/$branch/$relpath#L$line"
                        case gitea;   set url "$url/src/branch/$branch/$relpath#L$line"
                        case '*';     set url "$url/blob/$branch/$relpath#L$line"
                    end
                    xdg-open $url &>/dev/null &
                    return
                end
            end
            xdg-open $argv &>/dev/null &
    end
end
