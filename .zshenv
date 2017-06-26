ZDOTDIR=$HOME/.hidden/zsh

PATH=$PATH:$HOME/.local/bin

src=$HOME/src
tmp=$HOME/tmp
mvim_args='-np'

export LESSHISTFILE=$HOME/.local/var/.less_history
export EDITOR="mvim ${mvim_args} -f --nomru -c 'au VimLeave * !open -a iTerm'"
