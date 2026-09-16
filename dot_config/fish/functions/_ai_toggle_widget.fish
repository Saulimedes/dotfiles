# Ctrl+O: toggle between current command line and AI suggestion from opencode.
# First press: sends the current line as a prompt, replaces with suggestion.
# Second press (with suggestion active): restores the original.
# Third press (with original active and cached suggestion): re-applies suggestion.
function _ai_toggle_widget
    set -l current (commandline)
    test -z "$current"; and return

    # Toggle back to original if we're showing the cached suggestion
    if set -q _ai_suggestion; and test "$current" = "$_ai_suggestion"
        commandline -- $_ai_original
        commandline -f repaint
        return
    end

    # Toggle back to suggestion if we're showing the original
    if set -q _ai_suggestion; and test "$current" = "$_ai_original"
        commandline -- $_ai_suggestion
        commandline -f repaint
        return
    end

    # New query: fetch from opencode
    set -g _ai_original $current
    set -e _ai_suggestion

    set -l result (opencode run --format json "$_AI_SYSTEM_PROMPT" $current 2>/dev/null \
        | jq -r 'select(.type == "text") | .part.text' | tr -d '\n')

    if test -n "$result"
        set -g _ai_suggestion $result
        commandline -- $result
    end
    commandline -f repaint
end
