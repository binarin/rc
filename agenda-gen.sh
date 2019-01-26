#!/usr/bin/env bash
set -euxo pipefail

cd $HOME/.rc
git fetch origin master
# git reset --hard FETCH_HEAD
# git clean -dfx

cd $HOME/org
git fetch origin master
git reset --hard FETCH_HEAD


rm -rf ~/tmp/agenda-gen
mkdir -p ~/tmp/agenda-gen

rm -f /var/www/org/*.html

emacs -batch -l ~/.emacs -eval '(org-batch-store-agenda-views org-habit-show-habits nil)' -kill
cp ~/tmp/agenda-gen/*.html /var/www/org/

emacs -batch -l ~/.emacs -eval '(org-batch-store-agenda-views org-agenda-span (quote fortnight) org-habit-show-habits nil)' -kill
for file in ~/tmp/agenda-gen/*.html; do
    base=$(basename $file .html)
    cp $file /var/www/org/$base-14.html
done

echo "<ul>" > /var/www/org/index.html
for file in /var/www/org/*.html; do
    echo "<li><a href=$(basename $file)>$(basename $file)</a></li>" >> /var/www/org/index.html
done
echo "</ul>" >> /var/www/org/index.html
