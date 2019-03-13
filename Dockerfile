from debian:latest

run apt-get update
run apt-get install -y make curl zsh git vim ruby

# install rust
workdir /root/tmp
run curl -O https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init
run chmod u+x rustup-init
run ./rustup-init -y --no-modify-path --default-toolchain nightly

# install haskell
run apt-get install -y ghc cabal-install

copy . /root/dots
workdir /root/dots
run ./infect

# core utils
run zsh -c "cargo install bat fd-find ripgrep"
run apt-get install -y cmake tmux autojump

run rm -rf /root/tmp

workdir /root
entrypoint zsh
