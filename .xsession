#!/bin/zsh
. ~/.zshrc

dropbox start || true
insync start || true
kbdd &
kupfer --no-splash &
eval $(ssh-agent)

while true; do 
xmonad
done
