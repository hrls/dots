ln -hf .gitconfig .global_ignore .vimrc .zshrc $HOME
# symlinx .zsh folder
rsync -rP .zsh $HOME
