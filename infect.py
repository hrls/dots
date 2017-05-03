import sys, os, os.path as path
etc = path.abspath('.etc')
sys.path.append(etc)
from sh import *

vss = [
    '.etc',
    '.gitconfig',
    '.gitignore',
    '.emacs',
    '.vimrc',
    '.zshrc',
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

# home/.emacs.d/
@restore_cwd
def install_emacs_pkgs():
    fp = p(home, '.emacs.d')
    ensure_dir(fp)
    cd(fp)

    def use_package():
        git_clone('https://github.com/jwiegley/use-package')

    @restore_cwd
    def haskell_mode():
        # https://github.com/haskell/haskell-mode#installation-from-git-repository
        git_clone('https://github.com/haskell/haskell-mode')
        cd('haskell-mode')
        shell('make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs')

    use_package()
    haskell_mode()

if __name__ == '__main__':
    ensure_dir(home)
    link_dots()
    # install_emacs_pkgs()

