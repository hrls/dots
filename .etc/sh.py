# import sys    
# sys.modules[__name__] = Sh(...)
import os, os.path as path

home = path.expanduser('~')
pwd = lambda: os.getcwd()
cd = lambda d: os.chdir(d)
is_there = lambda f: path.islink(f) or path.isfile(f)
def rm(f):
    if path.islink(f):
        os.unlink(f)
    elif path.isfile(f):
        os.remove(f)
git_clone = lambda repo: os.system('git clone {}'.format(repo))
shell = lambda s: os.system(s)

def p(*ls):
    return path.sep.join(ls)

def ensure_dir(d):
    if not path.isdir(d):
        os.makedirs(d)

def rel(base, tdir):
    return path.relpath(path.expanduser(base),
                        path.expanduser(tdir))

def restore_cwd(f):
    def wrap():
        cwd = pwd()
        f()
        cd(cwd)
    return wrap

