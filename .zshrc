alias cls='clear'
alias ll='ls -lAGh'
alias la='ls -A'
alias e='mvim'
alias pc='rsync -P'
alias py='python'
alias md5sum='md5 -r'

alias java6='export JAVA_HOME=$(/usr/libexec/java_home -v 1.6)'
alias java7='export JAVA_HOME=$(/usr/libexec/java_home -v 1.7)'

autoload -U colors && colors

# [hrls@probe /bin]$ default prompt
# [root@probe /var]# root prompt todo
PROMPT=$'%{\e[38;5;255m%}[%{\e[38;5;193m%}%n%{\e[38;5;75m%}@%{\e[38;5;193m%}%m %{\e[38;5;190m%}%~%{\e[38;5;255m%}]%{\e[38;5;178m%}$%{\e[0m%}'


export LC_CTYPE=en_US.UTF-8

fpath=( ~/.zsh "${fpath[@]}" )
autoload -U misc && misc
autoload -U add_env && add_env

# pkgsrc path
export PATH=/usr/pkg/sbin:/usr/pkg/bin:$PATH

# plan9 env
export PLAN9=/usr/local/plan9
export PATH=$PATH:$PLAN9/bin

export PATH=$PATH:$HOME/bin
