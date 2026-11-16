# Reminds you of an existing alias when you type out its expansion.
# Native port of zsh-you-should-use (aliases only, like the zsh setup).
status is-interactive; or exit

function _ysu_reload
    set -g _ysu_names
    set -g _ysu_values
    for line in (alias)
        set -l m (string match -r '^alias (\S+) (.*)$' -- $line); or continue
        set -a _ysu_names $m[2]
        set -a _ysu_values (string unescape -- $m[3])
    end
end

function _ysu_check --on-event fish_preexec
    set -q _ysu_names; or _ysu_reload
    set -l typed (string trim -- $argv[1])
    set -l first (string split -m1 ' ' -- $typed)[1]
    set -l best 0
    set -l best_len 0
    for i in (seq (count $_ysu_names))
        set -l value $_ysu_values[$i]
        set -l n (string length -- $value)
        test $n -gt $best_len; or continue
        test "$first" = "$_ysu_names[$i]"; and continue
        test (string sub -l $n -- $typed) = "$value"; or continue
        set -l rest (string sub -s (math $n + 1) -- $typed)
        test -z "$rest"; or string match -q ' *' -- $rest; or continue
        set best $i
        set best_len $n
    end
    test $best -gt 0; or return
    set_color --bold yellow
    echo -s 'Found existing alias for "' $_ysu_values[$best] '". You should use: "' $_ysu_names[$best] '"' >&2
    set_color normal
end
