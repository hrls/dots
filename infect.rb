#!/usr/bin/env ruby

require 'pathname'
require 'fileutils'

# TODO: cd pwd func wrapper
# TODO: sshh module @home, shell, github().pull/clone

# Paths and utils
module Base
  def home
    Pathname.new('~').expand_path
  end

  def newhome
    p(home) unless home.exist?
  end

  def p(base, *subs)
    dir = Pathname.new(base).join(*subs).expand_path
    FileUtils.mkdir_p(dir) unless dir.exist?
    dir
  end

  module_function :home, :newhome, :p

  def github(repo)
    local = Pathname.new(repo.split('/').last)
    if local.exist? && (local / '.git').exist?
      system("git -C #{local.realpath} pull")
    else
      system("git clone https://github.com/#{repo}")
    end
  end
end

# Vim plugins
class Vim
  extend Base
  @pkgs = %w[

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

  def self.packages
    autoload_dir = p(home / '.vim' / 'autoload')
    pathogen = autoload_dir / 'pathogen.vim'
    system("curl -LSso #{pathogen} https://tpo.pe/pathogen.vim")

    bundles = p(home / '.vim' / 'bundle')
    FileUtils.cd(bundles, verbose: true)
    @pkgs.each { |pkg| github pkg }
  end

  def self.helptags
    system('vim -es +Helptags +exit')
  end
end

# Env / shell / etc
class Env
  extend Base

  @linked = %w[
    makefile
    .gitconfig
    .gitignore
    .vimrc
    .zshrc
    .emacs
    .hidden

    .tcshrc
    .irbrc
    .ghci
    .tmux.conf
    .psqlrc
  ]

  def self.links
    pre = Pathname.pwd.relative_path_from(home)
    @linked.each do |fname|
      FileUtils.ln_s(pre + fname, home, force: true, verbose: true)
    end
  end

  def self.hushlogin
    hl = home / '.hushlogin'
    system("touch #{hl}") unless hl.exist?
  end

  def self.dotlocal
    p(home / '.local' / 'bin')
    p(home / '.local' / 'var')
  end
end

# TODO: symlinks scripts/* ~/.local/bin

if __FILE__ == $PROGRAM_NAME
  # Modules.*.for_each(Module.*)
  Base.newhome

  Env.hushlogin
  Env.links
  Env.dotlocal

  Vim.packages
  Vim.helptags
else
  p(home) # shutup rubocop
  # TODO: prompt lists available modules and their commands
  #       wildcards for interactive launch with 'irb -I . -r infect' => 'Env.*'
end
