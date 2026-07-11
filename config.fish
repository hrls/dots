set -gx src $HOME/src

function path -a dir
    set -gx PATH $PATH $dir
end

# Terminal tab/window title.
# Usage:
#   st my project      set the title to "my project"
#   dt                 set the title to the current dir basename
#   set -e custom_title  clear the title (falls back to shortened pwd)
# Notes:
#   - fish calls fish_title before each prompt and wraps the output in the
#     OSC set-title escape itself, so we just echo the text (no raw escapes).
#   - zsh used `t` to set the title, but here `t` is `alias t tree`, so `st`.
function fish_title
    if set -q custom_title[1]
        echo -- $custom_title
    else
        echo -- (prompt_pwd)
    end
end

function st
    set -gx custom_title $argv
end

function dt
    set -gx custom_title (basename $PWD)
end

path /opt/homebrew/bin
path $HOME/.cargo/bin
path $HOME/.local/bin

path $HOME/.local/share/solana/install/active_release/bin

set -gx EDITOR nvim
set -gx LSCOLORS Exfxcxdxbxegedabagacad

if status is-interactive
    alias a 'ls -oAhFG'
    alias g git
    alias e nvim
    alias t tree
    alias car cargo
    alias r 'nnn -HoUC'
    # alias unset 'set -e'

    alias cdr 'cd (git rev-parse --show-toplevel)'

    [ -f /opt/homebrew/share/autojump/autojump.fish ]; and source /opt/homebrew/share/autojump/autojump.fish

    # https://github.com/junegunn/fzf
    fzf --fish | source
    set -gx FZF_DEFAULT_OPTS '--no-separator --tabstop=4' 
    set -gx FZF_CTRL_T_COMMAND 'fd --type file --type directory --hidden --follow'
    # TODO: C-t must respect path like `cat ~/src/` then C-t shows relative to 
    # bind ctrl-j fzf-cd-widget # TODO: fix gnvi false-fire when Sublime Merge open it
end


source ~/.env.fish 2>/dev/null
