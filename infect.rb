require 'pathname'
require 'fileutils'

# todo: cd pwd func wrapper
# todo: sshh module @home, shell, github().pull/clone

def github repo
    local = Pathname.new(repo.split('/').last)
    if local.exist? and local.join('.git').exist?
        system "git -C #{local.realpath} pull"
    else
        system "git clone https://github.com/#{repo}"
    end
end

def pdir base, *subs
    dir = Pathname.new(base).join(*subs).expand_path
    FileUtils.mkdir_p dir unless dir.exist?
    dir
end

module Vim extend self
    @@pkgs = %(

        hrls/bullfinch
        vim-airline/vim-airline

        junegunn/fzf.vim
        majutsushi/tagbar
        scrooloose/nerdtree
        vim-scripts/restore_view.vim

        tpope/vim-markdown

    ).split

    def install_packages home: '~'
        home = Pathname.new(home).expand_path unless home.instance_of? Pathname

        autoload_dir = pdir home, '.vim', 'autoload'
        system "curl -LSso #{autoload_dir.join('pathogen.vim').to_s} https://tpo.pe/pathogen.vim"

        bundle_dir = pdir home, '.vim', 'bundle'
        FileUtils.cd bundle_dir, verbose: true
        @@pkgs.each do |pkg|
            github pkg
        end
    end
end

home = Pathname.new('~/tmp/newhome').expand_path

pdir home, '.local', 'bin'
pdir home, '.local', 'var'

# todo: touch ~/.hushlogin
# todo: link dotfiles
# todo: symlinks scripts/* ~/.local/bin

Vim.install_packages home: home
