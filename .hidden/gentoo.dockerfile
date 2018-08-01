from gentoo/stage3-amd64

run emerge --sync
run eselect news read
run emerge          \
    sys-devel/make  \
    net-misc/curl   \
    app-shells/zsh  \
    dev-vcs/git     \
    dev-lang/ruby   \
    app-editors/vim

run chsh --shell /bin/zsh

run eselect locale set en_US.utf8
run eselect editor set vi

# entrypoint zsh

# install dots
workdir /root/src
run git clone https://github.com/hrls/dots
workdir /root/src/dots
run ./infect

run emerge                  \
    app-misc/tmux           \
    app-misc/jq             \
    sys-fs/ncdu             \
    net-analyzer/gnu-netcat \
    dev-vcs/tig


run curl https://sh.rustup.rs -sSf | sh -s -- -y --no-modify-path --default-toolchain nightly
run cargo install ripgrep
