" https://learnvimscriptthehardway.stevelosh.com

" disables 24-bit colors (Terminal.app doesn't support them)
" TODO: sync value with 0.10 updates
set notermguicolors

set smoothscroll
set mousescroll=ver:1,hor:1 " the same as C-e/y
set nowrap
set number
set cursorline

" dialogs
set confirm
set shortmess=aOTI

" tabs, but spaces
set smartindent
set smarttab
set expandtab
set tabstop=4
set shiftwidth=4

" swiper
" TODO: show the preview window
set incsearch
set hlsearch
set ignorecase
set smartcase
set shortmess+=s
" C-g/t for next/prev match when search pattern edit is active

set laststatus=2
set statusline=%F
" set statusline+=%#LineNr#
set statusline+=%{StatuslineGit()}

set noshowmode " cursor indicates mode
set ruler
set wildmenu
set wildchar=<tab>

set splitbelow
set splitright

set nobackup
set nowritebackup
set noswapfile

set novisualbell

set guicursor=i-r-c-ci-cr:hor25
autocmd VimLeave,VimSuspend * set guicursor=a:hor25-blinkon1 " restore system cursor

set title
auto BufEnter * let &titlestring = expand("%:.") " TODO: use project root


function! CustomTitle()
    try
        let l:git_root = GitRoot()
        let b:custom_title = l:git_root
    catch
        let l:dir = fs.basename(getcwd())
        let l:filename = expand("%:t")
        let b:custom_title = l:dir .. l:filename
    endtry

    echomsg 'buffer title is:' .. b:custom_title
endfunction


" bindings
" :h <LocalLeader>
" C-h (dup of Backspace) is ready to use
nnoremap / /\v
nnoremap ? ?\v
nnoremap <C-s> :write<Enter>
imap <C-s> <Esc>/

" jump around wrapped text
nnoremap j gj
nnoremap k gk

" copy to system clipboard (default mapping on Windows is redo)
" TODO: yY should acts like 'copy the whole line into pasteboard'
" state('o') operator pending is can be used for this catch
vnoremap Y "+y 
vnoremap D "+d

noremap <leader>8 :nohlsearch<Enter>
" :let @/ = "" " to clear the last used search pattern

" help pages have useful map `gO` for table of contents

" Window management
" Num C-S-6 switches to buffer #Num
noremap <leader>4 <C-w>o


" Project-related (git, find path)
set wildignore+=*/.git/*

set autochdir

" TODO: netrw should set title
let g:netrw_keepdir = 0

" TODO: doesnt works but as idea for future impl
" autocmd BufReadPost * lua require("config.bufferline_setup").close_empty_unnamed_buffers()
" autocmd VimLeavePre * if s:isActiveSession() && s:saveSession()

" https://neovim.io/doc/user/russian.html#russian.txt
set langmap="ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz"

autocmd TermOpen * setlocal nonumber | startinsert
" TODO: echo works in terminal but not in nvim
autocmd VimLeave * silent !echo -ne "\e[3 q"


lua require('my')

" lua require('haunt')
" luafile 'haunt'
" command! Haunt lua haunt()

" TODO:
" tune :h WORD for prog-mode
" plugin idea: C-Enter to complete all unclosed }).
" clean previous message after next move or so


runtime packages.vim
runtime lua/git.lua
