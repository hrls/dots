from archlinux:latest

run pacman --sync --noconfirm --refresh
run pacman --sync --noconfirm \
    core/make \
    core/curl \
    core/gcc \

    extra/zsh \
    extra/git \
    extra/cmake \
    extra/bat \
    extra/fd \
    extra/ripgrep \
    extra/neovim \

    extra/tmux \
    extra/ruby \
    extra/htop \
    extra/fzf \
    extra/jq \
    extra/ncdu \
    extra/tldr \
    extra/tokei \
    extra/github-cli

# rm 'NoExtract' line with man path from /etc/pacman.conf
# pacman -Sy --noconfirm man man-db man-pages

run chsh --shell /bin/zsh
entrypoint zsh

copy . /src/dots
workdir /src/dots
run ruby infect.rb

# rust-lang via https://rustup.rs
run curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- -y --no-modify-path --default-toolchain stable

run source ~/.cargo/env && rustc --version

workdir /tmp
