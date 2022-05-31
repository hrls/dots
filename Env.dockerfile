from archlinux:latest

run pacman --sync --noconfirm --refresh
run pacman --sync --noconfirm \
    core/curl \
    extra/zsh \
    extra/git \
    extra/ruby \
    extra/emacs \
    extra/htop \
    community/tmux \
    community/bat \
    community/fd \
    community/ripgrep \
    community/fzf \
    community/jq \
    community/ncdu \
    community/tldr \
    community/tokei \
    community/github-cli

run chsh --shell /bin/zsh
entrypoint zsh

copy . /src/dots
workdir /src/dots
run ruby infect.rb
