#!/bin/zsh
. ~/.zshrc
HOST=$(hostname)
if [[ $HOST == demandred ]]; then
    xrandr --output DP1 --auto --primary --left-of HDMI2 --output HDMI2 --auto --rotate left --dpi 160
fi

CUSTOM_XRESOURCES_FILE="~/.rc/.Xresources.$HOST"
if [[ -f $CUSTOM_XRESOURCES_FILE ]]; then
    xrdb -merge $CUSTOM_XRESOURCES_FILE
fi

which dropbox > /dev/null && dropbox start
which insync > /dev/null && insync start
which yandex-disc > /dev/null && yandex-disc start

which kupfer > /dev/null && kupfer --no-splash &

which ssh-agent > /dev/null && has_ssh_agent_binary=1
which gpg-agent > /dev/null && has_gpg_agent_binary=1
which keychain > /dev/null && has_keychain_binary=1

if [[ $has_keychain_binary -gt 0 ]]; then
    keychain_agents=ssh
    if [[ $has_gpg_agent_binary -gt 0 ]]; then
        keychain_agents="$keychain_agents,gpg"
    fi
    eval $(keychain --eval --agents "$keychain_agents" --quiet)
else
    if [[ $has_ssh_agent_binary -gt 0 ]]; then
        eval $(ssh-agent)
    fi
    if [[ $has_gpg_agent_binary -gt 0 ]]; then
        eval $(gpg-agent --daemon)
    fi
fi

while true; do 
    xmonad
done
