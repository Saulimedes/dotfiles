function ai
    if test -z "$argv"
        echo "Usage: ai <prompt>"
        return 1
    end

    set -l result (opencode run --format json "$_AI_SYSTEM_PROMPT" "$argv" 2>/dev/null \
        | jq -r 'select(.type == "text") | .part.text' | tr -d '\n')

    if test -z "$result"
        echo "No response from opencode"
        return 1
    end

    commandline $result
end
