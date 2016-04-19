# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

ZSH_THEME="nebirhos"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

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

BIN_DIRS=(
$HOME/bin
$HOME/.local/bin
$HOME/.cabal/bin
$HOME/apps/*/bin(N)
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

alias gl='git log  --pretty="%Cgreen%h %C(146)%an%Creset %s %Cred%ar"'
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

for candidate in emacsclient vim vi; do
    if [[ ! -z $(which $candidate) ]]; then
        export VISUAL=$candidate
        export EDITOR=$candidate
        break
    fi
done

export DEBFULLNAME="Alexey Lebedeff"
export DEBEMAIL="binarin@binarin.ru"
alias dch='dch --vendor=debian'

rr() {
    readlink -f $(which $1)
}

function sudoedit() {
    /usr/bin/emacsclient -nw /sudo:root@localhost:$1
}
