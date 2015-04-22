#!/bin/zsh
cd ~/.rc
git submodule update --init

if [ ! -d ~/.oh-my-zsh ]; then
    git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
fi

cd ~
cp -s ~/.rc/{.emacs,.xmobarrc,.zshrc,.vimrc} .
ln -s ~/.rc/.urxvt .

mkdir -p ~/.vim/colors
ln -s ~/.rc/vim/zenburn/colors/zenburn.vim ~/.vim/colors

mkdir -p ~/bin
cd ~/bin
cp -s ~/.rc/{workrave-break.sh,xmobar-clock-monitor.sh,xmobar-workrave-monitor.sh,zsh-remote-setup} .

mkdir -p ~/.xmonad
cd ~/.xmonad
cp -s ~/.rc/.xmonad/xmonad.hs .
ln -s ~/.rc/.xmonad/lib ~/.xmonad/lib
