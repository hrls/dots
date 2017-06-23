LC_CTYPE=en_US.UTF-8

alias p=echo
alias cls='clear'
alias del='rm'
alias which='which -a'
alias ll='ls -lAFGHh'
alias la='ls -AFG'
alias cp='cp -a'

alias g=git
alias e="mvim ${mvim_args} --"
alias er="mvim ${mvim_args} -R --"

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

# erlang
alias erlich='erl -man'
alias erl_tags='find . -type f -iname "*.[he]rl" | etags -'

# stack
alias sr='stack ghci'
alias sc='stack clean'
alias sb='stack build'
alias se='stack exec'

alias df='df -H'
alias top='top -o cpu'
alias ips='ifconfig | grep inet'
alias pc='rsync -Ph' # -P same as --partial --progress
alias md5sum='md5 -r'
alias ra='ranger'
alias btli="btcli list | grep -e '[LI+]\.\s'"
alias ltr="py ~/.hidden/ltr.py"
alias ww="qlmanage -p $@ >& /dev/null"

autoload -U colors && colors
# todo: replace ANSI by supported xterm-256color
LSCOLORS='Exfxcxdxbxegedabagacad'
export CLICOLOR_FORCE=true
alias less='less -r'
alias more='more -r'

# [hrls@probe /bin]$ default prompt
# [root@probe /var]# root prompt todo
# user @ machine  ~ λ
# todo [[ $SHELL -ne 'dumb' ]]
if [[ $TERM != 'dumb' ]] then
    PROMPT=$'%{\e[38;5;255m%}[%{\e[38;5;193m%}%n%{\e[38;5;75m%}@%{\e[38;5;193m%}%m %{\e[38;5;190m%}%~%{\e[38;5;255m%}]%{\e[38;5;178m%}$%{\e[0m%}'
fi

bindkey -v # vim
# todo: zle vim mode
# https://github.com/hrls/dots/commit/c4453bc987d388d233ec5af597cffea580c3f71e#diff-ec20fb240e117fea7b0049c21edf1ef3

fpath=( ~/.hidden/zsh '${fpath[@]}' )
autoload -U misc && misc
autoload -U add_env && add_env
autoload -U haskell && haskell
autoload -U ejabberd && ejabberd
[ -f ~/.hidden/zsh/tmux ] && source ~/.hidden/zsh/tmux


# db*
add_postgres

# batteries
eval "$(thefuck --alias)"
[[ -f /usr/local/etc/profile.d/autojump.sh ]] && . /usr/local/etc/profile.d/autojump.sh

[[ -f ~/.private ]] && source ~/.private
