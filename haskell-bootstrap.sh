#!/bin/bash
set -ex
mkdir -p ~/apps/haskell/bin

# Generate standalone binaries in clean environment - so no problems with conflicting deps
for package in yeganesh hoogle hindent hlint structured-haskell-mode; do
    docker run -v $HOME/apps/haskell/bin:/root/.cabal/bin --rm -i -t haskell:7.8.4 bash -c "cabal update && cabal install $package"
done

docker run -v $HOME/apps/haskell/bin:/root/.cabal/bin --rm -i -t haskell:7.8.4 bash -c "export DEBIAN_FRONTEND=noninteractive; apt-get update && apt-get install -y xorg-dev libxml2-dev libasound2-dev c2hs libiw-dev && cabal update && cabal install xmobar --flags=all_extensions"
