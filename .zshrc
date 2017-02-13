bindkey -v
export KEYTIMEOUT=10

alias p=print
alias cls='clear'
alias del='rm'
alias which='which -a'
alias ll='ls -lAFGHh'
alias la='ls -AFG'
alias df='df -H'

alias e='mvim -np --'
alias ra='ranger'
alias top='top -o cpu'
alias ips='ifconfig | grep inet'

# python
alias py='python3'
alias pip='pip3'

# stack
alias sr='stack ghci'
alias sc='stack clean'
alias sb='stack build'
alias se='stack exec'

# tmux
alias q='tmux attach || tmux new'

alias pc='rsync -Ph' # -P same as --partial --progress
alias md5sum='md5 -r'
alias btli="btcli list | grep -e '[LI+]\.\s'"
alias ww="qlmanage -p $@ >& /dev/null"
# alias ww='mvim -R'

# alias java9='export JAVA_HOME=$(/usr/libexec/java_home -v 1.9)'

autoload -U colors && colors
export LSCOLORS='Exfxcxdxbxegedabagacad' # replace ANSI by supported xterm-256color
export CLICOLOR_FORCE=true
alias less='less -r'
alias more='more -r'

# [hrls@probe /bin]$ default prompt
# [root@probe /var]# root prompt todo
PROMPT=$'%{\e[38;5;255m%}[%{\e[38;5;193m%}%n%{\e[38;5;75m%}@%{\e[38;5;193m%}%m %{\e[38;5;190m%}%~%{\e[38;5;255m%}]%{\e[38;5;178m%}$%{\e[0m%}'

export src=$HOME/src
export tmp=$HOME/tmp

export LC_CTYPE=en_US.UTF-8

fpath=( ~/.etc/zsh "${fpath[@]}" )
autoload -U misc && misc
autoload -U add_env && add_env

# pkgsrc path
# export PATH=/usr/pkg/sbin:/usr/pkg/bin:$PATH

# JVM
# java8
# add_clj

# db
add_pgs

# export GEM_HOME=$HOME/.gems
# export PATH=$GEM_HOME/bin:$PATH

export PATH=$PATH:$HOME/.local/bin

export EDITOR='mvim -f --nomru -c "au VimLeave * !open -a iTerm"'

[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh

# if [[ $TERM != "screen" ]] then
    # ( (tmux has-session -t remote && tmux attach-session -t remote) || (tmux new-session -s remote) ) && exit 0
    # echo "error: tmux failed to start"
# fi
