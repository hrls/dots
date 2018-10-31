export LANG=en_US.UTF-8
export LC_ALL=C.UTF-8

ZDOTDIR=$HOME/.hidden/zsh

PATH="$PATH:/usr/local/sbin"
[[ -d "$HOME/.local/bin" ]] && PATH=$PATH:"$HOME/.local/bin"
[[ -d "$HOME/.cargo/bin" ]] && PATH=$PATH:"$HOME/.cargo/bin"

src=$HOME/src
tmp=$HOME/tmp

export LESSHISTFILE=$HOME/.local/var/.less_history
export REDISCLI_HISTFILE=$HOME/.local/var/.rediscli_histfile

[[ -f ~/.private ]] && source ~/.private
