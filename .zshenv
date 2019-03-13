export LANG=en_US.utf8

export LC_ALL=en_US.UTF-8
# export LC_ALL=en_US.utf8

ZDOTDIR=$HOME/.hidden/zsh

PATH="$PATH:/usr/local/sbin"
[[ -d "$HOME/.local/bin" ]] && PATH=$PATH:"$HOME/.local/bin"
[[ -d "$HOME/.cargo/bin" ]] && PATH=$PATH:"$HOME/.cargo/bin"
[[ -d "$HOME/.cabal/bin" ]] && PATH=$PATH:"$HOME/.cabal/bin"

src=$HOME/src
tmp=$HOME/tmp

export LESSHISTFILE=$HOME/.local/var/.less_history
export REDISCLI_HISTFILE=$HOME/.local/var/.rediscli_histfile

export BAT_THEME='1337'
export BAT_STYLE='plain,numbers,changes'

# export FZF_DEFAULT_COMMAND='git ls-files || fd --type file'

[[ -f ~/.private ]] && source ~/.private
