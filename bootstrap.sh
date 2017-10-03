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

cp -sn ~/.rc/{.emacs,.xmobarrc,.zshrc,.vimrc,.Xresources,.xsession,.sbclrc,.tmux.conf,.gitconfig,.quiltrc,.mbsyncrc,.conkerorrc,.dircolors,.aspell.en.pws,.rtorrent.rc} .
if [ ! -e ~/.urxvt ]; then
    ln -s ~/.rc/.urxvt .
fi

mkdir -p ~/.vim/colors
if [ ! -e ~/.vim/colors/zenburn.vim ]; then
    ln -s ~/.rc/vim/zenburn/colors/zenburn.vim ~/.vim/colors
fi

mkdir -p ~/bin
cd ~/bin
cp -sn ~/.rc/{workrave-break.sh,xmobar-clock-monitor.sh,xmobar-workrave-monitor.sh,zsh-remote-setup,conkeror,toggle-touchpad,volume.sh,sshmenu} .

mkdir -p ~/.xmonad
cd ~/.xmonad
cp -sn ~/.rc/.xmonad/xmonad.hs .

mkdir -p ~/.config/taffybar
cd ~/.config/taffybar
cp -sn ~/.rc/taffybar/taffybar.hs .

if [ ! -e ~/.xmonad/lib ]; then
    ln -s ~/.rc/.xmonad/lib ~/.xmonad/lib
fi

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
