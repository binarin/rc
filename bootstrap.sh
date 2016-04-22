#!/usr/bin/env zsh
set -e
cd ~/.rc
git submodule update --init

if [ ! -d ~/.oh-my-zsh ]; then
    git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

if [ ! -d ~/personal-workspace/org-mode ]; then
    git clone git://orgmode.org/org-mode.git ~/personal-workspace/org-mode
fi

if [ ! -f ~/personal-workspace/org-mode/lisp/org.elc ]; then
    make 'ORG_ADD_CONTRIB=*' -C ~/personal-workspace/org-mode
fi

if [ ! -d ~/personal-workspace/edts ]; then
    git clone https://github.com/tjarvstrand/edts ~/personal-workspace/edts
    make -C ~/personal-workspace/edts
fi

cd ~

cp -sn ~/.rc/{.emacs,.xmobarrc,.zshrc,.vimrc,.Xresources,.xsession,.sbclrc,.tmux.conf,.gitconfig,.quiltrc,.mbsyncrc,.conkerorrc} .
if [ ! -e ~/.urxvt ]; then
    ln -s ~/.rc/.urxvt .
fi

mkdir -p ~/.vim/colors
if [ ! -e ~/.vim/colors/zenburn.vim ]; then
    ln -s ~/.rc/vim/zenburn/colors/zenburn.vim ~/.vim/colors
fi

mkdir -p ~/bin
cd ~/bin
cp -sn ~/.rc/{workrave-break.sh,xmobar-clock-monitor.sh,xmobar-workrave-monitor.sh,zsh-remote-setup,conkeror} .

mkdir -p ~/.xmonad
cd ~/.xmonad
cp -sn ~/.rc/.xmonad/xmonad.hs .

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

# vagrant plugin install vagrant-libvirt
