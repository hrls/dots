ZDOTDIR=$HOME/.hidden/zsh

[[ -d "$HOME/.local/bin" ]] && PATH=$PATH:"$HOME/.local/bin"
[[ -d "$HOME/.cargo/bin" ]] && PATH=$PATH:"$HOME/.cargo/bin"

src=$HOME/src
tmp=$HOME/tmp

export LESSHISTFILE=$HOME/.local/var/.less_history

[[ -f ~/.private ]] && source ~/.private
