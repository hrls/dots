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
alias e="mvim ${mvim_args}"
alias er="mvim ${mvim_args} -R"

alias grep='grep --color=auto -E'			# egrep
# -Hn file:lineno
# grep_find: find . -type f -exec grep  -nH -e  {} +
grey() {
	find . -type f -name $1 -exec egrep -Hn -e $2 {} + # todo
}
	
# alias grey='grep -Er --include=\*.{h,e}rl "record" .'

# python
alias py='python3 -B'
alias pyre='py -i'
alias pip='pip3'

alias tags='ctags -R'

# erlang
alias erlich='erl -man'
alias erl_tags="ctags -R ./src ./deps/*/src"
# alias erl_tags='find . -type f -iname "*.[he]rl" | etags -'

# stack
alias sr='stack ghci'
alias sc='stack clean'
alias sb='stack build'
alias se='stack exec'

alias df='df -H'
alias top='t ∆ && top -o cpu; dir_title'
alias ips='ifconfig | grep inet'
alias pc='rsync -Ph' # -P same as --partial --progress
alias md5sum='md5 -r'
alias ra='t 🏹 && ranger; dir_title'
alias btli="btcli list | grep -e '[LI+]\.\s'"
alias ltr="py ~/.hidden/ltr.py"
alias ww="qlmanage -p $@ >& /dev/null"

autoload -U colors && colors
# todo: replace ANSI by supported xterm-256color
LSCOLORS='Exfxcxdxbxegedabagacad'
export CLICOLOR_FORCE=true
alias less='less -r'
alias more='more -r'

setopt nobeep
# setopt menucomplete
# zstyle ':completion:*' menu select=1 _complete _ignored _approximate

t = title () {
    print -Pn "\033];$@\a"
    # http://www.refining-linux.org/archives/42/ZSH-Gem-8-Hook-function-chpwd/
}
dir_title() {
    if [[ $SHLVL == 1 ]] then
        case `basename $PWD` in
            hrls)
                t '~' ;;
            dots)
                t '…' ;;
            *)
                t '•' ;;
        esac
    fi
}
# http://www.faqs.org/docs/Linux-mini/Xterm-Title.html#ss4.1
# https://www-s.acm.illinois.edu/workshops/zsh/prompt/escapes.html
chpwd_functions=(${chpwd_functions[@]} 'dir_title')
wrap_ss() { return 'todo: prepend space before function call' }
# todo: fix detached HEAD
git_head() {
    ref_head=`git symbolic-ref HEAD 2>/dev/null | cut -d / -f 3`
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
if [[ $TERM != 'dumb' ]] then
    bindkey -v
    # todo: custom root prompt
    # todo: prepend or rprompt user@host %{\e[38;5;249m%}%n%{\e[38;5;75m%}@%{\e[38;5;249m%}%m
    setopt prompt_subst
    PROMPT=$'%{\e[38;5;195m%}%~%{\e[38;5;222m%}$(git_head) %{\e[38;5;176m%}λ %{\e[0m%}'

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
[[ -f /usr/local/etc/profile.d/autojump.sh ]] && . /usr/local/etc/profile.d/autojump.sh

[[ -f ~/.private ]] && source ~/.private

# post hooks
clear
dir_title
