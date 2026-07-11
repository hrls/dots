from debian:latest

run apt update
run apt upgrade -y
run apt install -y zsh bat fd-find ripgrep autojump git neovim tmux gcc ruby make cmake curl rustup
run rustup default stable

# copy . /root/dots
# workdir /root/dots
# run ./infect.rb

workdir /root

entrypoint ["/bin/zsh"]
