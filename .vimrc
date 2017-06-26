" todo:
" NERD_Tree: aliases 'e[r] $file' edit / RO
" zsh :!aliases
"
"
"

set nocompatible
set viminfo+=n~/.local/var/viminfo

execute pathogen#infect()
syntax on
filetype plugin indent on

set encoding=utf-8

color bullfinch

" Menlo:12 Apple font based on Bitstream Vera and Deja Vu
" Monaco:12 Apple font default in iTerm2
" Inconsolata:14 Open source font by Raph Levien
" http://blog.fnurl.se/2011/06/22/comparison-bitstream-vs-dejavu-vs-menlo/
" http://www.fontsquirrel.com/fonts/Inconsolata
set guifont=Menlo:h12

set shortmess=atI

set laststatus=2    " vim-airline
set noshowmode      " try shm+=s
set ruler
set wildmenu        " =list:longest
set showcmd

set number
set cursorline

set nowrap

set autoindent
set smartindent
set smarttab
set expandtab
set tabstop=4
set shiftwidth=4


set incsearch
set hlsearch
set ignorecase
set smartcase
" set replace global by default, just use :s/foo/bar/ instead of :s/foo/bar/g
set gdefault

" set showmatch " hl [{()}]

set ttyfast

set visualbell " no fucking beeps; macvim dont flashes
set guioptions+=c " kill popup dialogs
set guioptions-=m " menu
set guioptions-=T " toolbar
set guioptions-=L " left scrollbar

set splitbelow
set splitright

let mapleader = ";"
nmap \ <leader>

" binds
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" navigate around wrapped text
nnoremap j gj
nnoremap k gk

nnoremap <tab> <c-w>w
nnoremap <s-tab> <c-w>W

" todo: bypass warning - E16: Invalid range
noremap <D-1> :1tabnext<cr>
noremap <D-2> :2tabnext<cr>
noremap <D-3> :3tabnext<cr>
noremap <D-4> :4tabnext<cr>
noremap <D-5> :5tabnext<cr>
inoremap <D-1> <Esc>:1tabnext<cr>
inoremap <D-2> <Esc>:2tabnext<cr>
inoremap <D-3> <Esc>:3tabnext<cr>
inoremap <D-4> <Esc>:4tabnext<cr>
inoremap <D-5> <Esc>:5tabnext<cr>

vnoremap < <gv
vnoremap > >gv

" highlights w/o jump to the next occurrence
nnoremap * *N
" highlights selected in vmode
" just highlight, select in vmode, then replace :'<,'>s//bar/g
vnoremap * y :execute ":let @/=@\""<cr> :execute "set hlsearch"<cr>
" turn off searh highlight
nnoremap <leader>8 :nohlsearch<cr>

" folds
set foldenable
set foldmethod=indent
set foldlevelstart=10
set foldnestmax=10

" Plugins

" airline
" let g:airline_left_sep = '>'

" tagbar
nmap <f8> :TagbarToggle<cr>

" https://github.com/vim-scripts/restore_view.vim
set viewoptions=cursor,folds,slash,unix
" let g:skipview_files = ['*\.vim']

" https://github.com/scrooloose/nerdcommenter
let g:NERDSpaceDelims = 1

" https://github.com/scrooloose/nerdtree
" autocmd vimenter * if !argc() | NERDTree | endif
map <c-n> :NERDTreeToggle<cr>
let g:NERDTreeShowHidden = 1
" let g:NERDTreeShowBookmarks = 1
" <c-n> replace empty buffer (as ':e folder')
" close vim if the only window left is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" https://github.com/ctrlpvim/ctrlp.vim
set wildignore+=*/.git/*
let g:ctrlp_show_hidden = 1
" let g:ctrlp_user_command = 'find %s -type f'

" batteries
source $VIMRUNTIME/ftplugin/man.vim " :Man

" VimL
nnoremap <leader>s :so $VIMRUNTIME/syntax/hitest.vim<cr>

" Haskell
set wildignore+=*/.stack-work/*
autocmd BufWritePost package.yaml silent !hpack --silent

" Erlang
set wildignore+=*.beam

" etc...
" todo: term ≈ !open -a iTerm :pwd
" todo: autocomple function: fb<tab> -> FooBar .hs, foo_bar .erl

set runtimepath+=~/.hidden
runtime suwayyah.vim
