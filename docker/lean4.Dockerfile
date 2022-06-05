from archlinux:latest

run pacman --sync --noconfirm --refresh
run pacman --sync --noconfirm curl

run curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf \
    | sh -s -- -y --no-modify-path --default-toolchain leanprover/lean4:stable

run source ~/.elan/env && lean --version
