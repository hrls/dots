import sys, os, os.path as path
hidden = path.abspath('.hidden')
sys.path.append(hidden)
from sh import *

vss = [
    '.hidden',
    '.gitconfig',
    '.gitignore',
    '.vimrc',
    '.zshenv',
    '.psqlrc',
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
    ensure_dir(p(home, '.vim/autoload'))
    shell('curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim')

    # ~/.vim/bundle
    bundle_dir = p(home, '.vim/bundle')
    ensure_dir(bundle_dir)
    cd(bundle_dir)
    github('hrls/bullfinch')
    github('vim-airline/vim-airline')
    github('majutsushi/tagbar')
    github('ctrlpvim/ctrlp.vim')
    github('scrooloose/nerdtree')
    github('tpope/vim-fugitive')
    github('tpope/vim-markdown')
    github('elixir-lang/vim-elixir')
    github('vim-scripts/restore_view.vim')
    # github('tikhomirov/vim-glsl')

@restore_cwd
def touch_hushlogin():
    cd(home)
    touch('.hushlogin')

if __name__ == '__main__':
    # ['~', ['.local', ['bin', 'var']]]
    ensure_dir(p(home, '.local', 'bin'))
    ensure_dir(p(home, '.local', 'var'))

    link_dots()
    touch_hushlogin()
    install_vim_pkgs()
