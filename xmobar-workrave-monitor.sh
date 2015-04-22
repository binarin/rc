#!/bin/bash
set -ex

timer=$(dbus-send --print-reply=literal --type=method_call --dest=org.workrave.Workrave /org/workrave/Workrave/Core org.workrave.CoreInterface.GetTimerElapsed string:'restbreak')
regex='int32 ([0-9]+)'

if [[ $timer =~ $regex ]] ; then
    minutes=$((${BASH_REMATCH[1]} / 60))
    color=grey
    minutes_left=$((29-minutes))
    if [[ $minutes_left -lt 0 ]] ; then
        color='grey,red'
    fi
    echo "<fc=$color> [WR: $minutes_left] </fc>"
fi
