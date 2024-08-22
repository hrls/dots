export LANG=en_US.utf8
export LC_ALL=en_US.UTF-8 # macOS

# XDG-like Base Directory Specification
export CONFIG_HOME=$HOME/.config
export DATA_HOME=$HOME/.local/var

WORDCHARS=${WORDCHARS/\/} # rm path separator

# History
HISTFILE=$DATA_HOME/.zsh_history
SAVEHIST=4000
setopt inc_append_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
# setopt share_history

# https://superuser.com/questions/1610587/disable-zsh-session-folder-completely
export SHELL_SESSIONS_DISABLE=1

setopt nobeep

export src=$HOME/src
export tmp=$HOME/tmp

autoload -U colors && colors
export LSCOLORS='Exfxcxdxbxegedabagacad'
export CLICOLOR_FORCE=true

alias cls='clear' # tmux was there
alias which='whence -ac'
alias ll='ls -oAhH' # TODO: maybe --color as linux compat and then reverse with 'a'
alias l='ls -AhFG' # columns

# Definitely most used alias
# -o is new -l, just no group
# -A as -a but no meta dirs . ..
# -Fh some decorations
# -G color last as 'which a' then zle insert under cursor
alias a='ls -oAhFG'


alias more='less'
alias cp='cp -a'
alias fd='fd --hidden --color=auto'

alias tags='ctags -R'
alias s='pwd | pbcopy && echo "pbpaste ->" $PWD'
alias df='df -Hl'
alias du='du -d 1 -ch' # limit depth, human-readable, total
alias tp='titled ∆ htop --sort PERCENT_CPU'
alias ips='ifconfig | grep inet' # todo: filter loopback / inet6
alias pc='rsync -Ph' # -P same as --partial --progress
alias md5sum='md5 -r'
alias ra='titled 🗡️ nnn -HoUC'
alias py='python3 -B'
alias pyre='py -i'
alias e='nvim'

# Git aliases and typos
alias g=git
alias gs='git s'
alias gs.='git s .'
alias gdf='git df'
alias gdf.='git df .'
alias gl='git l'
alias gf='git f'
alias cdr='cd $(git rev-parse --show-toplevel)'

export EDITOR='nvim'

export LESS='--no-init --quit-if-one-screen --raw-control-chars --tabs=4'
export LESSHISTFILE=$DATA_HOME/.less_history

export REDISCLI_HISTFILE=$DATA_HOME/.rediscli_histfile


tabs -4
# man 1 zshmisc


# setopt menucomplete
# zstyle ':completion:*' menu select=1 _complete _ignored _approximate

t() { export custom_title=$@ && title }
dt() { export custom_title=`basename $PWD` && title }

# TODO: change tmux title
# http://www.refining-linux.org/archives/42/ZSH-Gem-8-Hook-function-chpwd/
title() {
    if (( $+custom_title ))
    then print -Pn "\033];${custom_title}\a"
    else print -Pn "\033];$@\a"
    fi
}

dir_title() {
    local pwd_name=`basename $PWD`
    case "$PWD" in
        "$HOME")             title '~' ;; 
        "$HOME/src/dots")    title '…' ;;
        *)                   title "$pwd_name" ;;
    esac
}

# TODO: resolve recursive calls (alias foo=titled f foo)
# new: 'titled ∆ top' add hooks precmd and set title
titled() {
    title $1 && eval ${@:2}; dir_title
}

# http://www.faqs.org/docs/Linux-mini/Xterm-Title.html#ss4.1
chpwd_functions=(${chpwd_functions[@]} 'dir_title')

function git_head() {
    local ref_head=`git symbolic-ref HEAD 2>/dev/null | cut -d / -f 3-`
    if [[ $ref_head != '' ]]; then
        echo " $ref_head"
    else
        tag=`git describe --exact-match HEAD 2>/dev/null`
        if [[ $? == 0 ]]; then
            echo " tag: $tag"
        else
            hc=`git rev-parse --short HEAD 2>/dev/null`
            if [[ $? == 0 ]]; then echo " head: $hc"; fi
        fi
    fi
}

# Forms user@host prefix for non-local environments.
# - empty for local env
# - hostname when username is hrls
# - full logname@hostname pair otherwise: ssh session, docker
function nonlocal_prefix() {
    local prefix=''
    if [[ $LOGNAME != 'hrls' ]]; then
        prefix+="$LOGNAME" # TODO: colorize
    fi
    # TODO: SSH connection prompt must be always in form of user@host
    if [[ -n ${prefix} || -v SSH_CONNECTION  ]]; then
        if [[ -n ${prefix} ]]; then
            prefix+='@'
        fi
        prefix+="$HOSTNAME"
    fi
    if [[ -n ${prefix} ]]; then
        prefix+=' '
    fi
    echo ${prefix}
}

if [[ $TERM != 'dumb' ]]; then
    bindkey -e
    # todo: custom root prompt
    # todo: prepend or rprompt user@host %{\e[38;5;249m%}%n%{\e[38;5;75m%}@%{\e[38;5;249m%}%m
    setopt prompt_subst
    PROMPT=$'$(nonlocal_prefix)%{\e[38;5;195m%}%~%{\e[38;5;222m%}$(git_head)%{\e[38;5;176m%} λ %{\e[0m%}'
fi


function load() { source "$HOME/.zsh/$1" }

# fpath+=~/.hidden/zsh
# todo: fpath / autoload / source

# autoload -U compinit && compinit
# zstyle ':completion:*descriptions' format '%U%B%d%b%u' # todo: tweak
# zstyle ':completion:*warnings' format 'no matches: %d%b'
# autoload -U promptinit && promptinit # todo: prompt -l

load zfuncs
load tmux
load docker; env_docker
load rust
load haskell
# load zty # TODO: incomplete af

# load erlang
# load envs
# env_postgres

case `uname -s` in
    Darwin) export os='macos' ;;
    Linux) export os='linux' ;;
    *) export os='unknown' ;;
esac

bindkey '^?' backward-delete-char

# Post hooks ...
[[ $SHLVL == 1 ]] && dir_title

[[ -f ~/.private ]] && source ~/.private
