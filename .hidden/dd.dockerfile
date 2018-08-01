from debian:latest

# pre
run apt update
# install base packages
run apt-get install -y curl make
run apt-get install -y zsh git ruby vim

# install dots
workdir /root/src
run git clone https://github.com/hrls/dots
workdir /root/src/dots
run ./infect

entrypoint zsh

# batteries
# TODO: fzf
run apt-get install -y autojump jq ncdu tmux
run apt-get install -y netcat screenfetch tig
run curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.8.1/ripgrep_0.8.1_amd64.deb
run dpkg -i ripgrep_0.8.1_amd64.deb

run apt-get install -y clang
run apt-get install -y ghc cabal-install
run curl https://sh.rustup.rs -sSf | sh -s -- -y --no-modify-path --default-toolchain nightly
# run cargo install mdcat
# run cargo install mdbook
