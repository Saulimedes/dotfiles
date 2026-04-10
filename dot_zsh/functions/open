# Helper: convert git remote URL to HTTPS browse URL
_open_remote_url() {
  local url
  url=$(git remote get-url origin 2>/dev/null) || return 1
  url=${url#git@}
  url=${url%.git}
  url=${url/://}
  [[ "$url" != https://* ]] && url="https://$url"
  echo "$url"
}

# Helper: detect forge type from remote URL
_open_forge_type() {
  local url
  url=$(git remote get-url origin 2>/dev/null) || return
  case "$url" in
    *gitlab*)                        echo "gitlab" ;;
    *codeberg*|*gitea*|*forgejo*)    echo "gitea" ;;
    *)                               echo "github" ;;
  esac
}

# Helper: current branch
_open_branch() {
  git symbolic-ref --short HEAD 2>/dev/null
}

# Helper: default branch with fallback
_open_default_branch() {
  local branch
  branch=$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | sed 's|origin/||')
  if [ -z "$branch" ]; then
    if git show-ref --verify --quiet refs/heads/main 2>/dev/null; then
      branch="main"
    elif git show-ref --verify --quiet refs/heads/master 2>/dev/null; then
      branch="master"
    fi
  fi
  echo "${branch:-main}"
}

open() {
  local forge url branch default_branch

  case "$1" in
    "")
      # No args: open repo in browser or current dir in file manager
      url=$(_open_remote_url) || { xdg-open . &>/dev/null &!; return; }
      forge=$(_open_forge_type)
      branch=$(_open_branch)
      default_branch=$(_open_default_branch)

      if [[ -n "$branch" && "$branch" != "$default_branch" ]]; then
        case $forge in
          gitlab) url="$url/-/tree/$branch" ;;
          gitea)  url="$url/src/branch/$branch" ;;
          *)      url="$url/tree/$branch" ;;
        esac
      fi
      ;;

    pr)
      url=$(_open_remote_url) || { echo "Not a git repo"; return 1; }
      forge=$(_open_forge_type)
      branch=$(_open_branch)
      default_branch=$(_open_default_branch)

      if [[ "$branch" == "$default_branch" ]]; then
        # On default branch: open PR/MR list
        case $forge in
          gitlab) url="$url/-/merge_requests" ;;
          *)      url="$url/pulls" ;;
        esac
      else
        # On feature branch: open new PR/MR page
        case $forge in
          gitlab) url="$url/-/merge_requests/new?merge_request[source_branch]=$branch" ;;
          github) url="$url/compare/$default_branch...$branch?expand=1" ;;
          gitea)  url="$url/compare/$default_branch...$branch" ;;
        esac
      fi
      ;;

    issues)
      url=$(_open_remote_url) || { echo "Not a git repo"; return 1; }
      case $(_open_forge_type) in
        gitlab) url="$url/-/issues" ;;
        *)      url="$url/issues" ;;
      esac
      ;;

    actions|ci)
      url=$(_open_remote_url) || { echo "Not a git repo"; return 1; }
      case $(_open_forge_type) in
        gitlab) url="$url/-/pipelines" ;;
        *)      url="$url/actions" ;;
      esac
      ;;

    *)
      # Check for file:line pattern
      if [[ "$1" =~ ^(.+):([0-9]+)$ ]] && [ -f "${match[1]}" ]; then
        local file="${match[1]}" line="${match[2]}"
        url=$(_open_remote_url) || { echo "Not a git repo"; return 1; }
        forge=$(_open_forge_type)
        branch=$(_open_branch)
        local relpath
        relpath=$(realpath --relative-to="$(git rev-parse --show-toplevel)" "$file")

        case $forge in
          gitlab) url="$url/-/blob/$branch/$relpath#L$line" ;;
          gitea)  url="$url/src/branch/$branch/$relpath#L$line" ;;
          *)      url="$url/blob/$branch/$relpath#L$line" ;;
        esac
      else
        # Default: pass through to xdg-open
        xdg-open "$@" &>/dev/null &!
        return
      fi
      ;;
  esac

  xdg-open "$url" &>/dev/null &!
}
