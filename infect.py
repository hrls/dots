import sys, os, os.path as path
etc = path.abspath('.etc')
sys.path.append(etc)
from sh import *

vss = [
    '.etc',
    '.gitconfig',
    '.gitignore',
    '.vimrc',
    '.zshenv',
    '.psqrc',
]

pre_rm = True

# home/.*
def link_dots():
    cwd = pwd()
    for e in vss:
        target = p(home, e)
        if pre_rm and is_there(target):
            rm(target)
        os.symlink(p(rel(cwd, home), e), target)

@restore_cwd
def install_vim_pkgs():
    # ~/.vim/autoload
    # todo: install / update pathogen ; .vim/autoload/pathogen.vim
    #       just download from github master and compare md5 hashes

    # ~/.vim/bundle
    bundle_dir = p(home, '.vim/bundle')
    ensure_dir(bundle_dir)
    cd(bundle_dir)
    github('hrls/bullfinch')
    github('ctrlpvim/ctrlp.vim')
    github('scrooloose/nerdtree')
    github('tpope/vim-markdown')
    github('elixir-lang/vim-elixir')
    github('vim-airline/vim-airline')
    github('vim-scripts/restore_view.vim')
    # github('tikhomirov/vim-glsl')

if __name__ == '__main__':
    # ['~', ['.local', ['bin', 'var']]]
    ensure_dir(p(home, '.local', 'bin'))
    ensure_dir(p(home, '.local', 'var'))
    link_dots()
    install_vim_pkgs()
