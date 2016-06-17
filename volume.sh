#!/usr/bin/env bash

case "$1" in
    up)
        pactl set-sink-volume 1 +5%
    ;;
    down)
        pactl set-sink-volume 1 -5%
    ;;
    mute)
        pactl set-sink-mute 1 toggle 
    ;;
esac
