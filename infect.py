import sys, os
etc = os.path.abspath('.etc')
sys.path.append(etc)
from sh import *

vss = [
    # '.etc/'
    '.gitconfig',
    '.gitignore',
    '.emacs',
    '.zshrc',
]

# home/.*
def link_dots():
    cwd = pwd()
    for e in vss:
        # if os.path.isfile(src):
        # if os.path.isfile(dst):
        #     os.remove(dst)
        os.symlink(p(fork_in_the_road(cwd, home), e), p(home, e))
        # elif os.path.isdir(src):
        # print('dir {}'.format(fpath))
        # os.symlink(e, '~', target_is_directory=

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
    install_emacs_pkgs()

