import os

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

if __name__ == '__main__':
    cwd = os.getcwd()
    home = os.path.expanduser('~')
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
