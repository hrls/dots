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
  dir.realpath
end

# TODO:
# - relax context as current cwd only
# - switch back from some branch to master / main when branch: nil
# - save updates into stash
def github(repo, branch: nil)
  local = Pathname.new(repo.split('/').last)

  unless (local / '.git').exist?
    git_clone = "git clone https://github.com/#{repo}"
    git_clone << " --branch #{branch}" unless branch.nil?
    return system git_clone
  end

  git = -> (sub, *mods) {
    command = "git -C #{local.realpath} #{sub}"
    if mods.any? :stdout
      stdout = `#{command}`
      stdout if $?.success? or throw :fail, command
    else
      system command
    end
  }

  git.call('fetch --all')
  current = git.call('branch --show-current', :stdout).strip

  # TODO: check refs, local branch must track remote one
  if branch.nil? || current == branch
    git.call('pull')
  else
    warn "current branch is #{current}, switching to #{branch}"
    git.call("checkout #{branch}")
    git.call("pull")
  end
end

def ln(path, to:)
  path = Pathname.new(path) unless path.is_a?(Pathname)
  return unless path.exist?

  prefix = path.realpath.dirname.relative_path_from(to)
  FileUtils.ln_s(prefix / path.basename, to, force: true, verbose: true)
  path
end

def lnx(of, to:)
  of = of.children if of.is_a?(Pathname) && of.directory?
  of.each.map { |piece| ln(piece, to: to) }
end

# TODO: cd pwd func wrapper
# TODO: sshh module @home, shell, github().pull/clone

This = Pathname.new(__FILE__).realpath.dirname
Home = Pathname.new('~').expand_path
# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
Config = dir(Home / '.config')
DataDir = dir(Home / '.local' / 'var')
Executables = dir(Home / '.local' / 'bin')


module Env
  Homies = This * %w[
    .gitconfig
    .gitignore

    .tmux.conf
    .ghci
    .irbrc
  ]

  def self.call
    lnx Homies, to: Home
  end

  def self.configs
    ln This / '.ripgreprc', to: Config
  end

  def self.extras
    ln This / '.psqlrc', to: Home
    ln This / '.tcshrc', to: Home
  end

  def self.hushlogin
    hushlogin = Home / '.hushlogin'
    system "touch #{hushlogin}" unless hushlogin.exist?
  end
end

module Zsh
  def self.call
    ln This / '.zshrc', to: Home
    lnx This / 'zsh', to: dir(Home / '.zsh')
  end
end

module Emacs
  Els = This / 'emacs'

  Init = Els / 'init.el'
  Themes = Els.glob('*-theme.el')
  Scripts = Els.glob('*.el') - ([Init] + Themes)

  Target = dir(Home / '.emacs.d')

  def self.init
    ln Init, to: Target
  end

  def self.themes
    lnx Themes, to: Target
  end

  def self.scripts
    lnx Scripts, to: dir(Target / 'elisp')
  end

  def self.patches
    FileUtils.cd(dir(Target / 'mods'), verbose: true)

    # https://github.com/flycheck/flycheck/pull/1948
    github "hrls/flycheck", branch: "fix/find-cargo-subcommand"
  end
end

module Vim
  Plugins = %w[

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

  def self.rc
    ln This / '.vimrc', to: Home
  end

  def self.plugins
    # install https://github.com/tpope/vim-pathogen
    autoload_dir = dir(Home / '.vim' / 'autoload')
    pathogen = autoload_dir / 'pathogen.vim'
    system "curl -LSso #{pathogen} https://tpo.pe/pathogen.vim"

    bundle = dir(Home / '.vim' / 'bundle')
    FileUtils.cd(bundle, verbose: true)
    Plugins.each { |plugin| github plugin }
  end

  def self.helptags
    system "vim -es +Helptags +exit"
  end
end


if __FILE__ == $PROGRAM_NAME
  Zsh.()

  Env.()
  Env.configs
  Env.extras
  Env.hushlogin

  Emacs.init
  Emacs.themes
  Emacs.scripts
  Emacs.patches

  Vim.rc
  Vim.plugins
  Vim.helptags

  lnx This / 'scripts', to: Executables
else
  # Modules.*.for_each(Module.*)
  # TODO: prompt lists available modules and their commands
  #       wildcards for interactive launch with 'irb -I . -r infect' => 'Env.*'
end
