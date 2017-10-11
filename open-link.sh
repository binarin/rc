#!/usr/bin/env zsh
local url="$1"

if [[ $url =~ 'booking\.com'
       || $url =~ 'booking\.facebook\.com'
       || $url =~ '#label/Lists%2Fsp\.data\.cron'
   ]]; then
    exec chromium "$@"
else
    exec firefox "$@"
fi
