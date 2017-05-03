# import sys    
# sys.modules[__name__] = Sh(...)
import os

home = os.path.expanduser('~/tmp/makabara')
pwd = lambda: os.getcwd()
cd = lambda d: os.chdir(d)
git_clone = lambda repo: os.system('git clone {}'.format(repo))
shell = lambda s: os.system(s)
def p(*ls):
    return os.path.sep.join(ls)
def ensure_dir(d):
    if not os.path.isdir(d):
        os.makedirs(d)

def restore_cwd(f):
    def wrap():
        cwd = pwd()
        f()
        cd(cwd)
    return wrap

