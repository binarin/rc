#!/usr/bin/env zsh
set -e
cd ~/.rc
git submodule update --init

if [ ! -d ~/.oh-my-zsh ]; then
    git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

# if [ ! -d ~/personal-workspace/org-mode ]; then
#     git clone git://orgmode.org/org-mode.git ~/personal-workspace/org-mode
# fi

# if [ ! -f ~/personal-workspace/org-mode/lisp/org.elc ]; then
#     make 'ORG_ADD_CONTRIB=*' -C ~/personal-workspace/org-mode
# fi

if [ ! -d ~/personal-workspace/edts ]; then
    git clone https://github.com/tjarvstrand/edts ~/personal-workspace/edts
    make -C ~/personal-workspace/edts
fi

cd ~

cp -sn ~/.rc/{.emacs,.xmobarrc,.zshrc,.vimrc,.xsession,.sbclrc,.gitconfig,.quiltrc,.mbsyncrc,.conkerorrc,.dircolors,.aspell.en.pws,.rtorrent.rc} .
if [ ! -e ~/.urxvt ]; then
    ln -s ~/.rc/.urxvt .
fi

mkdir -p ~/.vim/colors
if [ ! -e ~/.vim/colors/zenburn.vim ]; then
    ln -s ~/.rc/vim/zenburn/colors/zenburn.vim ~/.vim/colors
fi

mkdir -p ~/bin
cd ~/bin
cp -sn ~/.rc/{toggle-touchpad,volume.sh} .

mkdir -p ~/.xmonad
cd ~/.xmonad
cp -sf /etc/nixos/xmonad-config/build .

mkdir -p ~/.config/taffybar
cd ~/.config/taffybar
cp -sn ~/.rc/taffybar/taffybar.{hs,css} .

mkdir -p ~/.ghc
if [[ ! -e ~/.ghc/ghci.conf ]] ; then
    ln -s ~/.rc/ghci.conf ~/.ghc/ghci.conf
fi

if [[ ! -d ~/.tmux/plugins/tpm ]]; then
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
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

if [[ ! -e ~/.config/twmn/twmn.conf ]]; then
    mkdir -p ~/.config/twmn/
    ln -s ~/.rc/twmn.conf ~/.config/twmn/twmn.conf
fi

if [[ ! -e ~/.gnupg/gpg-agent.conf ]]; then
    mkdir -p ~/.gnupg
    cp -sn ~/.rc/gpg-agent.conf ~/.gnupg/
fi

mkdir -p ~/.workrave
touch ~/.workrave/workrave.ini
# vagrant plugin install vagrant-libvirt

if [[ ! -L ~/bin/Personal.kdbx ]]; then
    ln -sf ~/.rc/open-Personal.kdbx ~/bin/Personal.kdbx
fi

if [[ ! -f ~/.config/dunst/dunstrc ]]; then
    mkdir -p ~/.config/dunst/
    ln -sf ~/.rc/dunstrc ~/.config/dunst/dunstrc
fi

if [[ ! -f ~/.mplayer/config ]]; then
    mkdir -p ~/.mplayer
    ln -sf ~/.rc/mplayer.config ~/.mplayer/config
fi

mkdir -p ~/.config/pulse/
if [[ ! -f ~/.config/pulse/default.pa ]]; then
    ln -sf ~/.rc/default.pa ~/.config/pulse/default.pa
fi

mkdir -p ~/personal-workspace/ ~/booking-workspace/

# hack to disable dropbox updates
rm -rf ~/.dropbox-dist
install -dm0 ~/.dropbox-dist

if [[ !( -f ~/.config/user-dirs.dirs) || !(grep -q HOME/Downloads ~/.config/user-dirs.dirs) ]]; then
    mkdir -p ~/.config
    rm -f ~/.config/user-dirs.{dirs,locale}
    LANG=C xdg-user-dirs-update --force
fi

if [[ ! -f ~/.local/share/applications/smart-browser-chooser.desktop ]]; then
    mkdir -p ~/.local/share/applications
    ln -s ~/.rc/smart-browser-chooser.desktop ~/.local/share/applications/
    update-desktop-database ~/.local/share/applications
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

mkdir -p ~/.config/alacritty
ln -sf ~/.rc/alacritty.yml ~/.config/alacritty/alacritty.yml

mkdir -p ~/.config/broot
ln -sf ~/.rc/broot-conf.toml ~/.config/broot/conf.toml
