" https://github.com/junegunn/vim-plug#usage
"
" :PlugInstall
" :PlugClean
" :healthcheck

let g:plug_window='horizontal botright new'

call plug#begin()

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-lua/popup.nvim'

Plug 'nanotech/jellybeans.vim' " colorscheme

" TODO: re-use builtin treesitter
" - [ ] as this one have some error when navigating internal documentation 
" Plug 'nvim-treesitter/nvim-treesitter', { 'tag': 'v0.9.2', 'do': ':TSUpdate' }

" Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.5' }

Plug 'dense-analysis/ale'
" Plug 'ii14/neorepl.nvim' " it works, but not so usable for repl

" Plug 'junegunn/fzf'
" Plug 'junegunn/fzf.vim'
" Plug 'preservim/nerdcommenter'
" Plug 'Julian/lean.nvim'

call plug#end()

