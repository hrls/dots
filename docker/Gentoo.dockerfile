from gentoo/stage3

run emerge --sync --update --quiet
run emerge --quiet \
    app-shells/fish \
    app-editors/neovim \
    app-misc/tmux

run chsh root --shell /bin/fish
