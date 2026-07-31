set -gx src $HOME/src

function path -a dir
    set -gx PATH $PATH $dir
end

# Terminal tab/window title.
# Usage:
#   title my project   set the title to "my project"
#   dt                 set the title to the current dir basename
#   set -e custom_title  clear the title (falls back to auto)
# Auto: inside ~/src show the project root basename (~/src/foo/bar -> foo);
# dirs in $title_ns_dirs are namespaces, so ~/src/debridge/api/x -> api.
# Elsewhere fall back to shortened pwd.
# Note: fish calls fish_title before each prompt and wraps the output in the
# OSC set-title escape itself, so we just echo the text (no raw escapes).
# TODO: make src working the same way as there are no differences
set -g title_ns_dirs debridge

function fish_title
    if set -q custom_title[1]
        echo -- $custom_title
        return
    end
    if string match -q "$src/*" -- $PWD
        set -l parts (string split / -- (string replace -- "$src/" '' $PWD))
        if contains -- $parts[1] $title_ns_dirs; and set -q parts[2]
            echo -- $parts[2]
        else
            echo -- $parts[1]
        end
        return
    end
    echo -- (prompt_pwd)
end

function title
    set -gx custom_title $argv
end

function dt
    set -gx custom_title (basename $PWD)
end

# repeat the last command (zsh `r` = fc -e -); skips the `r` invocation itself,
# so it works whether or not fish has already pushed `r` onto $history.
function r --description 'repeat the last command'
    for cmd in $history
        if test (string trim -- $cmd) != r
            eval $cmd
            # TODO: exempt itself from the history
            return
        end
    end
    echo 'r: no previous command' >&2
    return 1
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
    alias car cargo
    alias carc 'cargo clippy'
    alias t 'nnn -HoUC'
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
