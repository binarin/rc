#!/usr/bin/env bash
set -euo pipefail

# GoldenDict refuses to read dictionaries symlinked by annex, so we are going to make a bunch of hardlinks instead
rm -rfv ~/.dicts
mkdir -p ~/.dicts
touch ~/.dicts/00_DONT_PUT_FILES_HERE_MANUALLY

cd ~/.dicts

for file in ~/annex/dictionaries/*; do
    name=$(basename "$file")
    real=$(readlink -f "$file")
    ln -T "$real" "$HOME/.dicts/$name"
done
