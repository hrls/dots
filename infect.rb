#!/usr/bin/env ruby

require 'pathname'
require 'fileutils'

class Pathname
  def *(subs)
    subs.map { |sub| self + sub }
  end
end

def dir(base, *subs)
  dir = Pathname.new(base).join(*subs).expand_path
  FileUtils.mkdir_p(dir) unless dir.exist?
  dir
end

# TODO: relax context as current cwd only
def github(repo)
  local = Pathname.new(repo.split('/').last)
  if local.exist? && (local / '.git').exist?
    system("git -C #{local.realpath} pull")
  else
    system("git clone https://github.com/#{repo}")
  end
end

def linked(path, to:)
  path = Pathname.new(path) unless path.is_a?(Pathname)
  prefix = path.realpath.dirname.relative_path_from(to)
  FileUtils.ln_s(prefix / path.basename, to, force: true, verbose: true)
  path
end

def make_links(of, to:)
  of = of.children if of.is_a?(Pathname) && of.directory?
  of.each.map { |piece| linked(piece, to: to) }
end

# TODO: cd pwd func wrapper
# TODO: sshh module @home, shell, github().pull/clone

This = Pathname.new(__FILE__).realpath.dirname
# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
Home = Pathname.new('~').expand_path
Config = dir(Home / '.config')
Executables = dir(Home / '.local' / 'bin')


module Env
  @rc_confs = This * %w[
    .gitconfig
    .gitignore

    .tmux.conf
    .ghci
    .irbrc
    .psqlrc

    .vimrc
    .tcshrc
  ]

  def self.call
    make_links(@rc_confs, to: Home)
  end

  def self.hushlogin
    hl = Home / '.hushlogin'
    system("touch #{hl}") unless hl.exist?
  end
end

module Zsh
  @sources = This * ['.zshrc', '.hidden']

  def self.call
    make_links(@sources, to: Home)
  end
end

module Emacs
  @els = This / 'emacs'
  @target = dir(Home / '.emacs.d')

  def self.init
    linked(@els / 'init.el', to: @target)
  end

  def self.themes
    make_links(@els.glob('*-theme.el'), to: @target)
  end

  def self.call
    init = self.init
    themes = self.themes

    processed = themes << init
    elisp = dir(@target / 'elisp')
    make_links(@els.glob('*.el') - processed, to: elisp)
  end
end

module Scripts
  @scripts = This / 'scripts'

  def self.call
    make_links(@scripts, to: Executables)
  end
end

module Vim
  @plugins = %w[

    hrls/bullfinch
    vim-airline/vim-airline

    wincent/ferret
    dyng/ctrlsf.vim
    w0rp/ale
    junegunn/fzf.vim
    scrooloose/nerdtree
    scrooloose/nerdcommenter
    majutsushi/tagbar

    vim-scripts/restore_view.vim

    rust-lang/rust.vim
    keith/swift.vim
    idris-hackers/idris-vim
    tpope/vim-markdown
    chr4/nginx.vim
  ]

  def self.plugins
    # install https://github.com/tpope/vim-pathogen
    autoload_dir = dir(Home / '.vim' / 'autoload')
    pathogen = autoload_dir / 'pathogen.vim'
    system("curl -LSso #{pathogen} https://tpo.pe/pathogen.vim")

    bundle = dir(Home / '.vim' / 'bundle')
    FileUtils.cd(bundle, verbose: true)
    @plugins.each { |plugin| github plugin }
  end

  def self.helptags
    system("vim -es +Helptags +exit")
  end
end


if __FILE__ == $PROGRAM_NAME
  Zsh.()

  Env.()
  Env.hushlogin

  Emacs.()
  Scripts.()

  # Vim.plugins
  # Vim.helptags
else
  # Modules.*.for_each(Module.*)
  # TODO: prompt lists available modules and their commands
  #       wildcards for interactive launch with 'irb -I . -r infect' => 'Env.*'
end
