#!/bin/sh
if command -v tmux &> /dev/null
then
  if [ ! -d ~/.tmux/plugins/tpm ] ; then
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm 
  else 
    cd ~/.tmux/plugins/tpm
    git pull
  fi
 exit
fi
