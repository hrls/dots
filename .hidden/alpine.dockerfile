from alpine:latest
arg with_rust=no
arg with_haskell=no

run apk update
run apk add curl make
run apk add zsh git ruby vim

# install dots
workdir /root/src
run git clone https://github.com/hrls/dots
workdir /root/src/dots
run ./infect

entrypoint zsh

# TODO: autojump
run apk add fzf jq ncdu tmux
run apk add netcat-openbsd tig

run if [[ ${with_rust} == "yes" ]]; then \
        apk add rust rust-doc cargo  \
    ;fi

# run apk add rust rust-doc cargo
# run cargo install ripgrep

# run apk add clang
run if [[ ${with_haskell} == "yes" ]]; then \
        apk add ghc cabal; \
    fi

