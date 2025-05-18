# ~/.config/fish/conf.d/fundle.fish

if not functions -q fundle
    curl -sL https://git.io/fundle-install | fish
end

# Plugin list
fundle plugin 'edc/bass'
fundle plugin 'evanlucas/fish-kubectl-completions'
fundle plugin 'kidonng/zoxide.fish'
fundle plugin 'jorgebucaran/autopair.fish'
fundle plugin 'realiserad/fish-ai'
fundle plugin 'franciscolourenco/done'
fundle plugin 'pymander/vfish'
fundle plugin 'pymander/plugin-emacs'

# Load plugins
fundle init
