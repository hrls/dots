" pathogen
execute pathogen#infect()
syntax on
filetype plugin indent on

set nocompatible
set encoding=utf-8

" todo: http://studiostyl.es/schemes/nightfall
color slate
" line numbers
set nu
set showcmd
set showmode
set ruler
" set relativenumber

set autoindent
set smartindent
set smarttab
set ts=4 sw=4 et

set ignorecase
set smartcase
" set replace global by default, just use :s/foo/bar/ instead of :s/foo/bar/g
set gdefault

set wildmenu
" set wildmode=list:longest

set ttyfast


" highlights search
set hlsearch

" highlights w/o jump to the next occurrence
nnoremap * *N

" highlights selected in vmode
vnoremap * y :execute ":let @/=@\""<cr> :execute "set hlsearch"<cr>

" just highlight, select in vmode, then replace :'<,'>s//bar/g

" disable highlighting by ctrl+f8
nnoremap <c-f8> :nohlsearch<cr>




" no fucking beep
set vb
" todo: need uniq prefix for my commands
" ========== temp section ==========
" open .vimrc for edit
" map ,vv :vsp $MYVIMRC<cr>
" reload config
" map ,vr :so $MYVIMRC<cr>
" ========== temp section ==========



" ========== mapping ==========
" turn off all arrows =)
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

"map! ii <Esc> " map ii to Esc
" tabs
nnoremap <c-tab> :tabnext<cr>
nnoremap <c-s-tab> :tabprevious<cr>
nnoremap <tab> <c-W>w
nnoremap <s-tab> <c-W>W
" tab reordering (not for osx)
" nnoremap <silent> <a-left> :execute 'silent! tabmove ' . (tabpagenr()-2)<cr>
" nnoremap <silent> <a-right> :execute 'silent! tabmove ' . tabpagenr()<cr>

" NERDTree
" open NERDTree automatically when vim starts up if no files were specified
" autocmd vimenter * if !argc() | NERDTree | endif
" open NERDTree
map <c-n> :NERDTreeToggle<cr>
" close vim if the only window left is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif


" Haskell mode
" use ghc functionality for haskell files
au Bufenter *.hs compiler ghc
" configure browser for haskell_doc.vim
let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"
