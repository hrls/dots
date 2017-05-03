# import sys    
# sys.modules[__name__] = Sh(...)
import os
from itertools import zip_longest, dropwhile

home = os.path.expanduser('~/tmp/newhome')
pwd = lambda: os.getcwd()
cd = lambda d: os.chdir(d)
git_clone = lambda repo: os.system('git clone {}'.format(repo))
shell = lambda s: os.system(s)

def p(*ls):
    return os.path.sep.join(ls)

def ensure_dir(d):
    if not os.path.isdir(d):
        os.makedirs(d)

def fork_in_the_road(src, dst):
    """
    rel path offset:
    /Users/foo/bar/baz /Users/foo/bar => baz
    /Users/foo/bar/baz /Users/foo/qux => ../baz
    """

    splitter = lambda dir_path: list(filter(lambda s: len(s) != 0, dir_path.split(os.path.sep)))
    src = splitter(src)
    dst = splitter(dst)

    comps = zip_longest(src, dst)
    undup = dropwhile(lambda t: t[0] == t[1], comps)
    by_comp = list(zip(*undup))
    none_filter = lambda it: filter(lambda v: v != None, it)
    downward = os.path.sep.join(none_filter(by_comp[0]))
    upward = os.path.sep.join(map(lambda _: '..', none_filter(by_comp[1])))
    if len(upward) == 0:
        return downward
    elif len(downward) == 0:
        return upward
    else:
        return p(upward, downward)

def restore_cwd(f):
    def wrap():
        cwd = pwd()
        f()
        cd(cwd)
    return wrap

