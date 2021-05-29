#!/usr/bin/env zsh
set -e
cd ~

cp -sn ~/.rc/{.emacs,.gitconfig} .
if [ ! -e ~/.urxvt ]; then
    ln -s ~/.rc/.urxvt .
fi

mkdir -p ~/bin
cd ~/bin
cp -sn ~/.rc/{toggle-touchpad,volume.sh} .

mkdir -p ~/.config/taffybar
cd ~/.config/taffybar
cp -sn ~/.rc/taffybar/taffybar.{hs,css} .

mkdir -p ~/.ghc
if [[ ! -e ~/.ghc/ghci.conf ]] ; then
    ln -s ~/.rc/ghci.conf ~/.ghc/ghci.conf
fi

if [[ ! -L ~/.ssh/config ]]; then
    if [[ -d ~/.ssh ]]; then
        mkdir -p ~/.ssh
    fi
    ln -sf ~/.rc/ssh_config ~/.ssh/config
fi

if [[ ! -d ~/.nixpkgs ]]; then
    mkdir ~/.nixpkgs
fi

if [[ ! -e ~/.nixpkgs/config.nix ]]; then
    ln -sf ~/.rc/config.nix ~/.nixpkgs/config.nix
fi

if [[ ! -e ~/.gnupg/gpg-agent.conf ]]; then
    mkdir -p ~/.gnupg
    cp -sn ~/.rc/gpg-agent.conf ~/.gnupg/
fi

mkdir -p ~/.workrave
touch ~/.workrave/workrave.ini

if [[ ! -f ~/.mplayer/config ]]; then
    mkdir -p ~/.mplayer
    ln -sf ~/.rc/mplayer.config ~/.mplayer/config
fi

mkdir -p ~/.config/pulse/
if [[ ! -f ~/.config/pulse/default.pa ]]; then
    ln -sf ~/.rc/default.pa ~/.config/pulse/default.pa
fi

mkdir -p ~/personal-workspace/ ~/booking-workspace/

# Force english names for user dirs
if [[ !( -f ~/.config/user-dirs.dirs) || !(grep -q HOME/Downloads ~/.config/user-dirs.dirs) ]]; then
    mkdir -p ~/.config
    rm -f ~/.config/user-dirs.{dirs,locale}
    LANG=C xdg-user-dirs-update --force || true
fi

if [[ ! -f ~/.local/share/applications/smart-browser-chooser.desktop ]]; then
    mkdir -p ~/.local/share/applications
    ln -s ~/.rc/smart-browser-chooser.desktop ~/.local/share/applications/
    update-desktop-database ~/.local/share/applications || true
fi

if [[ ! -f ~/.local/share/applications/org-protocol.desktop ]]; then
    mkdir -p ~/.local/share/applications
    ln -s ~/.rc/org-protocol.desktop ~/.local/share/applications/
    update-desktop-database ~/.local/share/applications
fi

if [[ ! -L ~/.local/share/applications/mimeapps.list ]] ;then
    rm -f ~/.local/share/applications/mimeapps.list
    ln -s ~/.config/mimeapps.list ~/.local/share/applications/mimeapps.list
fi

ln -sf ~/.rc/pass ~/bin/pass
