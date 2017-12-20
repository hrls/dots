ZDOTDIR=$HOME/.hidden/zsh

[[ -d "$HOME/.local/bin" ]] && PATH=$PATH:"$HOME/.local/bin"
[[ -d "$HOME/.cargo/bin" ]] && PATH=$PATH:"$HOME/.cargo/bin"

src=$HOME/src
tmp=$HOME/tmp

export LESSHISTFILE=$HOME/.local/var/.less_history
export REDISCLI_HISTFILE=$HOME/.local/var/.rediscli_histfile

[[ -f ~/.private ]] && source ~/.private
