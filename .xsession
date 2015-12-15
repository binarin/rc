#!/bin/zsh
. ~/.zshrc

dropbox start || true
insync start || true
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
