#!/bin/bash
if [[ $(emacsclient --e '(org-clocking-p)') == 't' ]] ; then
    echo "<fc=grey,#000000>"
else
    echo "<fc=white,#ff0000>"
fi
