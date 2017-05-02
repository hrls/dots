import os

pwd = os.getcwd
cd = os.chdir
sys = os.system

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

home = os.path.expanduser('~/tmp/newhome')

def restore_cwd(f):
    def wrap():
        cwd = pwd()
        f()
        cd(cwd)
    return wrap

# home/.*
def link_dots():
    cwd = pwd()
    for e in vss:
        src = cwd + os.path.sep + e
        dst = home + os.path.sep + e
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
    fp = home + os.path.sep + '.emacs.d'
    if not os.path.isdir(fp):
        os.makedirs(fp)
    os.chdir(fp)

    def use_package():
        sys('git clone https://github.com/jwiegley/use-package')

    @restore_cwd
    def haskell_mode():
        # https://github.com/haskell/haskell-mode#installation-from-git-repository
        sys('git clone https://github.com/haskell/haskell-mode')
        cd('haskell-mode')
        sys('make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs')

    use_package()
    haskell_mode()

if __name__ == '__main__':
    # todo: link .*
    install_emacs_pkgs()
