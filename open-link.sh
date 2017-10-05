#!/usr/bin/env zsh
local url="$1"

if [[ $url =~ 'booking\.com' ]]; then
    exec chromium "$@"
else
    exec firefox "$@"
fi
