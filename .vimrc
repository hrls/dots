set nocompatible

execute pathogen#infect()
syntax on
filetype plugin indent on

set encoding=utf-8

color bullfinch

" todo: NERD_Tree aliases
"  - `e/q $file` explore in read only  
"  - optional: sync zhs e/w aliases with 

" Menlo:12 Apple font based on Bitstream Vera and Deja Vu
" http://blog.fnurl.se/2011/06/22/comparison-bitstream-vs-dejavu-vs-menlo/
"
" Monaco:12 Apple font default in iTerm2
"
" Inconsolata:14 Open source font by Raph Levien
" http://www.fontsquirrel.com/fonts/Inconsolata
set guifont=Menlo:h12

" vim-airline
set laststatus=2
set ruler
set wildmenu
" set wildmode=list:longest
set showcmd
set showmode

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

" todo: bypass warning - E16: Invalid range
noremap <D-1> :1tabnext<CR>
noremap <D-2> :2tabnext<CR>
noremap <D-3> :3tabnext<CR>
noremap <D-4> :4tabnext<CR>
noremap <D-5> :5tabnext<CR>
inoremap <D-1> <Esc>:1tabnext<CR>
inoremap <D-2> <Esc>:2tabnext<CR>
inoremap <D-3> <Esc>:3tabnext<CR>
inoremap <D-4> <Esc>:4tabnext<CR>
inoremap <D-5> <Esc>:5tabnext<CR>

" highlights w/o jump to the next occurrence
nnoremap * *N

" highlights selected in vmode
vnoremap * y :execute ":let @/=@\""<cr> :execute "set hlsearch"<cr>

" just highlight, select in vmode, then replace :'<,'>s//bar/g

" turn off searh highlight
nnoremap <leader>8 :nohlsearch<cr>


" folds
set foldenable
set foldmethod=indent
set foldlevelstart=10
set foldnestmax=10

" turn off all arrows
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

" map! ii <Esc>
nnoremap <tab> <c-W>w
nnoremap <s-tab> <c-W>W

" Plugins

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
