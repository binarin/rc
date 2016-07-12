#!/usr/bin/env bash

sink=$(pactl list short sinks | grep $(LANG=C pacmd stat | grep -a 'Default sink name' | awk '{print $4}') | awk '{print $1}')

case "$1" in
    up)
        pactl set-sink-volume $sink +5%
    ;;
    down)
        pactl set-sink-volume $sink -5%
    ;;
    mute)
        pactl set-sink-mute $sink toggle
    ;;
esac
