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

" line numbers
set nu
set cul

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

set vb " no fucking beep
set go+=c " kill popup dialogs

set guioptions-=m " menu
set guioptions-=T " toolbar
set guioptions-=L " left scrollbar

set splitbelow
set splitright


" highlights w/o jump to the next occurrence
nnoremap * *N

" highlights selected in vmode
vnoremap * y :execute ":let @/=@\""<cr> :execute "set hlsearch"<cr>

" just highlight, select in vmode, then replace :'<,'>s//bar/g

" disable highlighting by ctrl+f8
nnoremap <c-f8> :nohlsearch<cr>


" FOLDing
" v{select}zf - collapse
" za - expand
set foldmethod=manual


map ; :

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

" tab reordering (not for osx)
" nnoremap <silent> <a-left> :execute 'silent! tabmove ' . (tabpagenr()-2)<cr>
" nnoremap <silent> <a-right> :execute 'silent! tabmove ' . tabpagenr()<cr>

" Plugins

" https://github.com/vim-scripts/restore_view.vim
set viewoptions=cursor,folds,slash,unix
" let g:skipview_files = ['*\.vim']

" https://github.com/scrooloose/nerdcommenter
let g:NERDSpaceDelims = 1

" NERDTree
" open NERDTree automatically when vim starts up if no files were specified
" autocmd vimenter * if !argc() | NERDTree | endif
" open NERDTree
map <c-n> :NERDTreeToggle<cr>
let g:NERDTreeShowHidden = 1
let g:NERDTreeShowBookmarks = 1
" close vim if the only window left is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif


" :Man
source $VIMRUNTIME/ftplugin/man.vim

" todo;
" NERD_tree hidden files
" <c-n> replace empty buffer (as 'e folder')

" VimL
nnoremap <leader>s :so $VIMRUNTIME/syntax/hitest.vim<cr>

" Haskell
autocmd BufWritePost package.yaml silent !hpack --silent

" Rust

