ZDOTDIR=$HOME/.hidden/zsh

[[ -d "$HOME/.local/bin" ]] && PATH=$PATH:"$HOME/.local/bin"

src=$HOME/src
tmp=$HOME/tmp
mvim_args="-np -c 'au VimLeave * !open -a iTerm'"

export LESSHISTFILE=$HOME/.local/var/.less_history
export EDITOR="mvim ${mvim_args} -f --nomru"
