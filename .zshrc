# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

ZSH_THEME="nebirhos"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
    autojump
    cabal
    colored-man
    compleat
    cpanm
    deb
    debian
    emacs
    git
    perl
    pip
)

if [[ $TERM == "dumb" ]]; then	# in emacs
    PS1='%(?..[%?])%!:%~%# '
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
else
    source $ZSH/oh-my-zsh.sh
    PROMPT="$HOST_PROMPT_$GIT_PROMPT"

    if [ -x /usr/bin/keychain ] ; then
       eval `keychain --eval -q`
    fi
fi

if [[ -d "$HOME/.pyenv" ]]; then
    export PYENV_ROOT="$HOME/.pyenv"
    $PYENV_ROOT/bin
    eval "$(pyenv init -)"
fi

BIN_DIRS=(
$HOME/bin
$HOME/.local/bin
$HOME/.cabal/bin
$HOME/apps/*/bin
)

# Remove dupes from 'path', which is array tied to 'PATH'
typeset -U path
for ((i=1; i<= $#BIN_DIRS; i++)) do
    for dir in $BIN_DIRS[i]; do
        if [ -d $dir ] ; then
            path=($dir "$path[@]")
        fi
    done
done

alias ack=ack-grep
alias a=ack-grep
alias gl='git log  --pretty="%Cgreen%h %C(146)%an%Creset %s %Cred%ar"'
alias v=xdg-open
alias vi='emacsclient -nw'
alias vim='emacsclient -nw'

HISTFILE=$HOME/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

autoload -U add-zsh-hook

if [[ -d $HOME/Dropbox ]] ; then
    SHELL_HISTORY_DIR=$HOME/Dropbox/shell-logs
else
    SHELL_HISTORY_DIR=$HOME/.shell-logs
fi
if [[ ! -d $SHELL_HISTORY_DIR ]] ; then
    mkdir -p $SHELL_HISTORY_DIR
fi

function log_commands_precmd() {
    if [ "$(id -u)" -ne 0 ]; then
        FULL_CMD_LOG="$SHELL_HISTORY_DIR/zsh-`hostname`-$USER-$(date -u "+%Y-%m-%d").log"
        echo "$USER@`hostname`:`pwd` [$(date -u)] `\history -1`" >> ${FULL_CMD_LOG}
    fi
}

# end and compare timer, notify-send if needed
function notifyosd-precmd() {
    if [ ! -z "$cmd" ]; then
        cmd_end=`date +%s`
        ((cmd_time=$cmd_end - $cmd_start))
    fi
    if [ ! -z "$cmd" -a $cmd_time -gt 10 -a -x /usr/bin/notify-send ]; then
	notify-send -i utilities-terminal -u low --expire-time 3000 --hint int:transient:1 "$cmd_basename completed" "\"$cmd\" took $cmd_time seconds"
    fi
    cmd=
}

# get command name and start the timer
function notifyosd-preexec() {
    cmd=$1
    cmd_basename=${cmd[(ws: :)1]}
    cmd_start=`date +%s`
}

add-zsh-hook precmd log_commands_precmd
add-zsh-hook precmd notifyosd-precmd
add-zsh-hook preexec notifyosd-preexec

export ANDROID_SDK_HOME=~/apps/android-sdk-linux
export ANDROID_HOME=~/apps/android-sdk-linux

if [[ -f ~/perl5/perlbrew/etc/bashrc ]] ; then
    source ~/perl5/perlbrew/etc/bashrc
fi

if [[ -f /etc/direct/direct_shell_rc ]] ; then
    source /etc/direct/direct_shell_rc
    alias m=/usr/local/bin/direct-sql
    alias ppc='/usr/local/bin/direct-sql devtest:ppc:all'
    alias pp1='/usr/local/bin/direct-sql devtest:ppc:1'
    alias pp2='/usr/local/bin/direct-sql devtest:ppc:2'
    alias pp3='/usr/local/bin/direct-sql devtest:ppc:3'
    alias pp4='/usr/local/bin/direct-sql devtest:ppc:4'
    alias pp5='/usr/local/bin/direct-sql devtest:ppc:5'
    alias pp6='/usr/local/bin/direct-sql devtest:ppc:6'
fi

for candidate in emacsclient vim vi; do
    if [[ ! -z $(which $candidate) ]]; then
        export VISUAL=$candidate
        export EDITOR=$candidate
        break
    fi
done

export DEBFULLNAME="Alexey Lebedeff"
export DEBEMAIL="binarin@yandex-team.ru"
alias dch='dch --vendor=debian'

if [ -e /home/binarin/.nix-profile/etc/profile.d/nix.sh ]; then . /home/binarin/.nix-profile/etc/profile.d/nix.sh; fi
