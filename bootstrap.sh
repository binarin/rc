#!/bin/zsh
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

cp -sn ~/.rc/{.emacs,.xmobarrc,.zshrc,.vimrc,.Xresources,.xsession,.sbclrc,.tmux.conf,.gitconfig,.quiltrc,.mbsyncrc} .
if [ ! -e ~/.urxvt ]; then
    ln -s ~/.rc/.urxvt .
fi

mkdir -p ~/.vim/colors
if [ ! -e ~/.vim/colors/zenburn.vim ]; then
    ln -s ~/.rc/vim/zenburn/colors/zenburn.vim ~/.vim/colors
fi

mkdir -p ~/bin
cd ~/bin
cp -sn ~/.rc/{workrave-break.sh,xmobar-clock-monitor.sh,xmobar-workrave-monitor.sh,zsh-remote-setup} .

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
