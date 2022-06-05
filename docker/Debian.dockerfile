from debian:latest

run apt-get update
run apt-get install -y make curl zsh git vim ruby

# install rust
workdir /root/tmp
run curl -O https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init
run chmod u+x rustup-init
run ./rustup-init -y --no-modify-path --default-toolchain nightly
run . /root/.cargo/env && cargo --version && rustc --version

# install haskell
run apt-get install -y ghc cabal-install
run ghc --version && cabal --version

copy . /root/dots
workdir /root/dots
run ./infect.rb

# core utils
run . /root/.cargo/env && cargo install bat fd-find ripgrep
run apt-get install -y cmake tmux autojump

workdir /root
run rm -rf /root/tmp

entrypoint zsh
