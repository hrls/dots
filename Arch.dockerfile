from archlinux:latest

run sed -i '/NoExtract\s*=\s*usr\/share\/man/d' /etc/pacman.conf

run pacman --noconfirm --sync --refresh # --sysupgrade
run pacman --noconfirm --sync \
    core/curl \
    core/make \
    core/man \
    core/man-pages \
    extra/zsh \
    extra/git \
    extra/emacs \
    extra/ruby \
    extra/htop \
    community/tmux \
    community/fd \
    community/bat \
    community/fzf \
    community/jq \
    community/ncdu \
    community/neovim \
    community/tldr \
    community/tokei \
    community/ranger \
    community/github-cli


# run pacman -Sy community/ghc community/cabal-install

run chsh --shell /bin/zsh
entrypoint zsh

copy . /src/dots
workdir /src/dots
run ruby infect.rb

workdir /root/tmp
run curl -O https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init
run chmod u+x rustup-init
run ./rustup-init -y --no-modify-path --default-toolchain nightly
run . /root/.cargo/env && cargo --version && rustc --version

workdir /root
run rm -rf /root/tmp
