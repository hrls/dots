ln -hf .gitconfig .global_ignore .hgrc .vimrc .zshrc $HOME
rsync -rP .zsh $HOME
