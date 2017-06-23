ZDOTDIR=$HOME/.etc/zsh

LESSHISTFILE=$HOME/.local/var/.less_history

PATH="/usr/local/opt/erlang@18/bin:$PATH"
PATH=$PATH:$HOME/.local/bin

src=$HOME/src
tmp=$HOME/tmp
mvim_args='-np'

export EDITOR="mvim ${mvim_args} -f --nomru -c 'au VimLeave * !open -a iTerm'"
