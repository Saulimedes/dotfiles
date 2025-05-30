if status is-interactive
    if type -q starship
        eval (starship init fish)
    end
end
