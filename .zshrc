# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

if [[ $TERM == "dumb" ]]; then	# in emacs
    PS1='%(?..[%?])%!:%~%# '
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
else
    # source $ZSH/oh-my-zsh.sh
    # PROMPT="$HOST_PROMPT_$GIT_PROMPT"
    # if [ -x /usr/bin/keychain ] ; then
    #    eval `keychain --eval -q`
    # fi
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
        if [[ $candidate == emacsclient ]]; then
            candidate="emacsclient -nw"
        fi
        export VISUAL=$candidate
        export EDITOR=$candidate
        break
    fi
done

eval $(dircolors --sh ~/.dircolors)

export DEBFULLNAME="Alexey Lebedeff"
export DEBEMAIL="binarin@binarin.ru"
alias dch='dch --vendor=debian'
alias di='docker run --rm -i -t'
alias rgrep='grep -R'
alias o='xdg-open'
alias pst='pstree -ap | less'

e() {
    local emacs_desktop_num=$(wmctrl -d | grep -oP '^(\d+)(?=.*emacs$)')
    if [[ ! -z "$emacs_desktop_num" ]]; then
        wmctrl -s "$emacs_desktop_num"
    fi

    if [[ $1 =~ ^(.*):([0-9]+)$ ]]; then
        emacsclient --no-wait +${match[2]} "${match[1]}"
    else
        emacsclient --no-wait "$@"
    fi

}

rr() {
    readlink -f $(which $1)
}

qap() {
    nix-env -qaP ".*${1}.*"
}

function sudoedit() {
    /usr/bin/emacsclient -nw /sudo:root@localhost:$1
}

kp() {
    local cluster=$(bkcloud get installations | grep '^\*' | awk '{print $6}' | tr ',' '\n' | grep -v -- '-m$' | fzf -1)
    bkcloud use cluster "$cluster"
    kubectl get pods | tail -n +2 | fzf -1 | awk '{print $1}'
}

kl() {
    local pod="$(kp)"
    local container=$(kubectl get pod $pod -o=jsonpath='{.spec.containers[*].name}' | tr ' ' '\n' | fzf -1)
    kubectl logs "$pod" -c "$container" "$@"
}

ke() {
    local cmd="${1:-bash}"
    local pod="$(kp)"
    local container=$(kubectl get pod $pod -o=jsonpath='{.spec.containers[*].name}' | tr ' ' '\n' | fzf -1)
    kubectl exec "$pod" -c "$container" -i -t -- "$cmd" "$@"
}

man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}

fzf_share=$(which fzf)
if type -p fzf-share > /dev/null ; then
    fzf_share=$(fzf-share)
    test -f "$fzf_share/completion.zsh" && . "$fzf_share/completion.zsh"
    test -f "$fzf_share/key-bindings.zsh" && . "$fzf_share/key-bindings.zsh"
fi

nixops() {
    NIXOPS_STATE=~/org/deployments.nixops $(whence -p nixops) "$@" --option extra-builtins-file /etc/nixos/nixops/extra-builtins.nix
}
