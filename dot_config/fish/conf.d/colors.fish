# Syntax colors approximating the zsh setup: zsh-syntax-highlighting defaults
# (commands green, reserved words and quotes yellow, errors bold red, paths
# underlined, globs blue) and zsh-autosuggestions' grey suggestion.
status is-interactive; or exit

set -g fish_color_normal normal
set -g fish_color_command green
set -g fish_color_keyword yellow
set -g fish_color_quote yellow
set -g fish_color_error red --bold
set -g fish_color_param normal
set -g fish_color_option normal
set -g fish_color_redirection normal
set -g fish_color_end normal
set -g fish_color_escape normal
set -g fish_color_operator blue
set -g fish_color_valid_path --underline
set -g fish_color_comment brblack
set -g fish_color_autosuggestion brblack
