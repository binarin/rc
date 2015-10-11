#!/bin/zsh
. ~/.zshrc

kbdd &
kupfer --no-splash &
eval $(ssh-agent)

while true; do 
xmonad
done
