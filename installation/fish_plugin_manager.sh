#!/bin/sh
if command -v fish &> /dev/null
then
    fish -c "curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher"
 exit
fi
