#!/usr/bin/env zsh
. ~/.zshrc
HOST=$(hostname)

fg-run() {
    which $1 > /dev/null && "$@"
}

bg-run() {
    fg-run "$@" &
}

start-programs() {
    fg-run drobpox start
    fg-run insync start
    fg-run yandex-disc start

    bg-run xscreensaver -nosplash
    bg-run dunst
    bg-run workrave
}

xmonad-loop() {
    rm -f $HOME/.xmonad/xmonad.state
    while true; do
        xmonad
    done
}

start-programs

systemctl --user stop nm-applet.service
systemctl --user start nm-applet.service

systemctl --user stop compton.service
systemctl --user stop taffybar.service
fg-run xrandr-auto configure
systemctl --user start compton.service
systemctl --user start taffybar.service

xmonad-loop
