import sys, os
etc = os.path.abspath('.etc')
sys.path.append(etc)
from sh import *

# todo:
# iterate dir files
# strip pwd from home prefix

vss = [
    '.gitconfig',
    '.gitignore',
    '.emacs',
    '.zshrc',
    '.etc/'
]

# home/.*
def link_dots():
    cwd = pwd()
    for e in vss:
        src = p(cwd, e)
        dst = p(home, e)
        if os.path.isfile(src):
            if os.path.isfile(dst):
                os.remove(dst)
            os.symlink(src, dst)
        elif os.path.isdir(src):
            # print('dir {}'.format(fpath))
            # os.symlink(e, '~', target_is_directory=
            pass

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
    # todo: link .*
    install_emacs_pkgs()

