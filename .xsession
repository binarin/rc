#!/usr/bin/env zsh
. ~/.zshrc
HOST=$(hostname)

fg-run() {
    which $1 > /dev/null && "$@"
}

bg-run() {
    fg-run "$@" &
}

set-dpi() {
    if [[ $HOST == demandred ]]; then
        xrandr --output DP1 --auto --primary --left-of HDMI2 --output HDMI2 --auto --rotate left --dpi 160
    elif [[ $HOST == lanfear ]]; then
        xrandr --output HDMI-0 --auto --right-of DisplayPort-0 --rotate left --output DisplayPort-0 --primary --auto
    fi
}

load-custom-xresources() {
    CUSTOM_XRESOURCES_FILE="~/.rc/.Xresources.$HOST"
    if [[ -f $CUSTOM_XRESOURCES_FILE ]]; then
        xrdb -merge $CUSTOM_XRESOURCES_FILE
    fi
}

start-programs() {
    fg-run drobpox start
    fg-run insync start
    fg-run yandex-disc start

    bg-run kupfer --no-splash
    bg-run xscreensaver -nosplash
    bg-run quasselclient
    bg-run skype
    bg-run dunst
}

setup-agents() {
    # gpg agent from 2.1 works fine without any keychain magic
    which gpg-agent > /dev/null && gpg-agent --daemon
    which keychain > /dev/null && eval $(keychain --eval --quiet) || eval $(ssh-agent)
}

xmonad-loop() {
    while true; do 
        xmonad
    done
}

set-dpi
load-custom-xresources
setup-agents
start-programs
xmonad-loop
