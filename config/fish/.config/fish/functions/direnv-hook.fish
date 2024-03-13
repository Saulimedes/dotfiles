# Run direnv on initial fish with fish_prompt event
# Support history arrows
# To override create direnv-hook function in your ~/.config/fish/functions
# Thanks to wderezin

function direnv-hook
    function __direnv_export
        command direnv export fish | source

        if test "$direnv_set_aliases" != "$DIRENV_ALIASES"
            for name in $direnv_alias_names
                functions --erase $name
            end
            set -e direnv_alias_names

            for cmd in (echo $DIRENV_ALIASES | string split --no-empty ':alias:')
                eval alias $cmd
                set parts (echo $cmd | string split --no-empty ' ')
                ! contains $parts[1] $direnv_alias_names; and set -g -a direnv_alias_names $parts[1]
            end
            set -q direnv_alias_names; and echo "direnv: alias " +$direnv_alias_names

            set -g direnv_set_aliases $DIRENV_ALIASES
        end
    end

    function __direnv_export_eval --on-event fish_prompt
        # Run on each prompt to update the state
        __direnv_export

        if test "$direnv_fish_mode" != "disable_arrow"
            # Handle cd history arrows between now and the next prompt
            function __direnv_cd_hook --on-variable PWD
                if test "$direnv_fish_mode" = "eval_after_arrow"
                    set -g __direnv_export_again 0
                else
                    # default mode (eval_on_pwd)
                    __direnv_export
                end
            end
        end
    end

    function __direnv_export_eval_2 --on-event fish_preexec
        if set -q __direnv_export_again
            set -e __direnv_export_again
            __direnv_export
            echo
        end

        # Once we're running commands, stop monitoring cd changes
        # until we get to the prompt again
        functions --erase __direnv_cd_hook
    end
end
