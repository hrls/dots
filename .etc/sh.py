# import sys    
# sys.modules[__name__] = Sh(...)
import os, os.path as path
from functools import update_wrapper

home = path.expanduser('~')
pwd = lambda: os.getcwd()
cd = lambda d: os.chdir(d)
is_there = lambda f: path.islink(f) or path.isfile(f)
shell = lambda s: os.system(s)

def rm(f):
    if path.islink(f):
        os.unlink(f)
    elif path.isfile(f):
        os.remove(f)

def p(*ls):
    return path.sep.join(ls)

def ensure_dir(d):
    if not path.isdir(d):
        os.makedirs(d)

def rel(base, tdir):
    return path.relpath(path.expanduser(base),
                        path.expanduser(tdir))

def restore_cwd(f, *args, **kwargs):
    def wrap_cdpwd(*args, **kwargs):
        cwd = pwd()
        y = f(*args, **kwargs)
        cd(cwd)
        return y
    return update_wrapper(wrap_cdpwd, f)


git = lambda cmd: os.system('git {}'.format(cmd))
git.clone = lambda repo: git('clone {}'.format(repo))
git.pull = lambda: git('pull')
# git.__class__.__getattribute__ = lambda self, cmd: self(cmd)

@restore_cwd
def github(repo):
    repo_dir = repo.split(path.sep)[1]
    if path.isdir(repo_dir):
        cd(repo_dir)
        git.pull()
    else:
        git.clone('https://github.com/{}'.format(repo))
