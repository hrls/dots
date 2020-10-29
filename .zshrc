export LANG=en_US.utf8
export LC_ALL=en_US.UTF-8 # macOS

# History
export HISTSIZE=200
export HISTFILE=$HOME/.local/var/.zsh_history
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups
setopt hist_ignore_space

setopt nobeep

PATH="$PATH:/usr/local/sbin"
[[ -d "$HOME/.local/bin" ]] && PATH=$PATH:"$HOME/.local/bin"
[[ -d "$HOME/.cargo/bin" ]] && PATH=$PATH:"$HOME/.cargo/bin"
[[ -d "$HOME/.cabal/bin" ]] && PATH=$PATH:"$HOME/.cabal/bin"

src=$HOME/src
tmp=$HOME/tmp

autoload -U colors && colors
export LSCOLORS='Exfxcxdxbxegedabagacad'
export CLICOLOR_FORCE=true

export BAT_THEME='1337'
export BAT_STYLE='plain,numbers,changes'


alias cls='clear'
alias which='which -a'
alias ll='ls -oAFHGh'
alias la='ls -AFG'
alias a='ll'
alias cp='cp -a'
alias fd='fd --hidden --color=auto'
alias grep=rg
alias rg='rg --hidden --color=auto'

alias cat=bat

alias g=git
alias gs='git s'
alias gl='git l'
alias gf='git f'
alias grom='git rebase --interactive origin/master'

alias tags='ctags -R'

alias s='pwd | pbcopy; exit'
alias vrg='rg --vimgrep --color=auto'
alias df='df -Hl'
alias tp='titled ∆ htop'
alias ips='ifconfig | grep inet' # todo: filter loopback / inet6
alias pc='rsync -Ph' # -P same as --partial --progress
alias md5sum='md5 -r'
alias ra='titled 🏹 ranger'
alias br=broot
alias py='python3 -B'
alias pyre='py -i'

alias less='less -r'
alias more='more -r'

# export FZF_DEFAULT_COMMAND='git ls-files || fd --type file'

export LESSHISTFILE=$HOME/.local/var/.less_history
export REDISCLI_HISTFILE=$HOME/.local/var/.rediscli_histfile


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
    # todo: check for 'probe' only too
    if [[ $HOST != 'lodb' && $HOST != 'lodb.local'
       && $HOST != 'pd' && $HOST != 'pd.local'
       ]]; then
        local host_pre="$HOST : "
    fi
    if [[ $PWD == $HOME ]]; then
        if (( $+host_pre ))
        then title "${host_pre}~"
        else title '~'
        fi
    else
        local pwd_name=`basename $PWD`
        case ${pwd_name} in
            dots)
                title "${host_pre}…" ;;
            *)
                if (( $+host_pre ))
                then title "${host_pre}${pwd_name}"
                else title "${pwd_name}"
                fi
        esac
    fi
}

# TODO: resolve recursive calls (alias foo=titled f foo)
# new: 'titled ∆ top' add hooks precmd and set title
titled() {
    title $1 && eval ${@:2}; dir_title
}

# http://www.faqs.org/docs/Linux-mini/Xterm-Title.html#ss4.1
chpwd_functions=(${chpwd_functions[@]} 'dir_title')

git_head() {
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

# TODO: fix
nonlocal_prefix() {
    echo "$USER@$HOST "
}

git_nstashes() {
    local n_stashes=`git stash list | wc -l`
}

if [[ $TERM != 'dumb' ]] then
    bindkey -e
    # todo: custom root prompt
    # todo: prepend or rprompt user@host %{\e[38;5;249m%}%n%{\e[38;5;75m%}@%{\e[38;5;249m%}%m
    setopt prompt_subst
    PROMPT=$'$(nonlocal_prefix)%{\e[38;5;195m%}%~%{\e[38;5;222m%}$(git_head) %{\e[38;5;176m%}λ %{\e[0m%}'

    # http://pawelgoscicki.com/archives/2012/09/vi-mode-indicator-in-zsh-prompt/
    vim_ins_mode="%{$fg[cyan]%}~%{$reset_color%}"
    vim_cmd_mode="%{$fg[green]%}≈%{$reset_color%}"
    vim_mode=$vim_ins_mode

    function zle-keymap-select {
      vim_mode="${${KEYMAP/vicmd/${vim_cmd_mode}}/(main|viins)/${vim_ins_mode}}"
      zle reset-prompt
    }
    zle -N zle-keymap-select

    function zle-line-finish {
      vim_mode=$vim_ins_mode
    }
    zle -N zle-line-finish

    function TRAPINT() {
      vim_mode=$vim_ins_mode
      return $(( 128 + $1 ))
    }

    non_null_retval() {
        retval=$?
        [[ $retval != 0 ]] && echo " =$retval"
    }
    # RPROMPT='$(non_null_retval) ${vim_mode}'
    # todo: remove rprompt; zle accept-line
    # http://www.howtobuildsoftware.com/index.php/how-do/1Em/zsh-zsh-behavior-on-enter
fi


function load() {
    source "$HOME/.hidden/zsh/$1"
}

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

# load erlang
# load envs
# env_postgres


case `uname -s` in
    Darwin)
        # TODO: switch back to Terminal.app if new frame was killed w/o switching
        export EDITOR="emacsclient --create-frame --no-wait --alternate-editor='open -a Emacs'"

        e() {
            case $# in
                0) (eval $EDITOR .) ;;
                *) (eval $EDITOR $@) ;; # TODO: emacsclient cant handle multiple files at once, try for loop
            esac
        }
        ;;
    Linux)
        export EDITOR='emacs -nw' # TODO: run as daemon
        ;;
esac

bindkey '^?' backward-delete-char

# Post hooks ...
[[ $SHLVL == 1 ]] && dir_title

# Batteries +[===]
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f /usr/local/etc/profile.d/autojump.sh ] && source /usr/local/etc/profile.d/autojump.sh

[[ -f ~/.private ]] && source ~/.private
