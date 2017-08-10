# init
if [[ `uname -s` == 'Darwin' && `/usr/bin/which mvim` != '' ]] then
    edi='mvim'
    edi_args="-np -c 'au VimLeave * !open -a iTerm'"
    edi_as_editor_args='-f --nomru'
else
    edi='vim'
fi
export EDITOR="${edi} ${edi_args} ${edi_as_editor_args}"

# todo :
# cursors
LC_CTYPE=en_US.UTF-8
HISTSIZE=80
HISTFILE=$HOME/.local/var/.zsh_history
SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups
setopt hist_ignore_space

alias p=echo
alias cls='clear'
alias del='rm'
alias which='which -a'
alias ll='ls -lAFGHh'
alias la='ls -AFG'
alias a='ll'
alias cp='cp -a'

alias g=git
alias e="${edi} ${edi_args}" # todo: 'e' as 'e .'
alias er="${edi} ${edi_args} -R"

# python
alias py='python3 -B'
alias pyre='py -i'
alias pip='pip3'

alias tags='ctags -R'

# stack
alias sr='stack ghci'
alias sc='stack clean'
alias sb='stack build'
alias se='stack exec'

alias df='df -H'
alias tp='titled ∆ top -o cpu'
alias ips='ifconfig | grep inet'
alias pc='rsync -Ph' # -P same as --partial --progress
alias md5sum='md5 -r'
alias ra='titled 🏹 ranger'
alias btli="btcli list | grep -e '[LI+]\.\s'"
alias ltr="py ~/.hidden/ltr.py"

autoload -U colors && colors
# todo: replace ANSI by supported xterm-256color
LSCOLORS='Exfxcxdxbxegedabagacad'
export CLICOLOR_FORCE=true
alias less='less -r'
alias more='more -r'

alias d=docker
alias dm=docker-machine
alias denv='eval $(docker-machine env)'

setopt nobeep
# setopt menucomplete
# zstyle ':completion:*' menu select=1 _complete _ignored _approximate

t () {
    export custom_title=$@
    title
}
title () {
    # http://www.refining-linux.org/archives/42/ZSH-Gem-8-Hook-function-chpwd/
    if (( $+custom_title ))
    then print -Pn "\033];${custom_title}\a" 
    else print -Pn "\033];$@\a"
    fi
}
dir_title () {
    case `basename $PWD` in
        hrls)
            title '~' ;;
        dots)
            title '…' ;;
        *)
            title '•' ;;
    esac
}
titled () {
    # todo: resolve recursive calls (alias foo=titled f foo)
    # new: 'titled ∆ top' add hooks precmd and set title
    title $1 && eval ${@:2}; dir_title
}
# http://www.faqs.org/docs/Linux-mini/Xterm-Title.html#ss4.1
# https://www-s.acm.illinois.edu/workshops/zsh/prompt/escapes.html
# todo: fix ctrl+c git prompt
chpwd_functions=(${chpwd_functions[@]} 'dir_title')
wrap_ss() { return 'todo: prepend space before function call' }

git_head() {
    local ref_head=`git symbolic-ref HEAD 2>/dev/null | cut -d / -f 3`
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

nonlocal_prefix() {
    if [[ $USER != 'hrls' && $HOST != 'probe.local' ]]; then
        echo "$USER@$HOST "
    fi
}

git_nstashes() {
    local n_stashes=`git stash list | wc -l`
}

if [[ $TERM != 'dumb' ]] then
    bindkey -v
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
        if [[ $retval != 0 ]]; then echo ": $retval"; fi
    }
    # RPROMPT='${vim_mode} $(non_null_retval)'
    # todo: remove rprompt; zle accept-line
    # http://www.howtobuildsoftware.com/index.php/how-do/1Em/zsh-zsh-behavior-on-enter
fi

bindkey '^B' push-line

function load() {
    absp="$HOME/.hidden/zsh/$1"
    [[ -f ${absp} ]] && source ${absp}
}

load misc
load add_env
load haskell
load erlang
load tmux

# fpath+=~/.hidden/zsh
# todo: fpath / autoload / source

# db*
add_postgres

# batteries
# autoload -U compinit && compinit
# zstyle ':completion:*descriptions' format '%U%B%d%b%u' # todo: tweak
# zstyle ':completion:*warnings' format 'no matches: %d%b'
# autoload -U promptinit && promptinit # todo: prompt -l
eval "$(thefuck --alias)"
eval "$(direnv hook zsh)" # todo: replace by [[ -f .env ]] customs instead of bash based direnv
[[ -f /usr/local/etc/profile.d/autojump.sh ]] && . /usr/local/etc/profile.d/autojump.sh

[[ -f ~/.private ]] && source ~/.private

# post hooks
if [[ $SHLVL == 1 ]] then
    clear
    dir_title
fi
