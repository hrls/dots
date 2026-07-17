" https://learnvimscriptthehardway.stevelosh.com

" TTY settings
" TODO: sync value with 0.10 updates
set notermguicolors " disables 24-bit colors (Terminal.app doesn't support them)

" Mices and Trackballs
" TODO: enable Trackpad horizontal scrolling in normal and visual mode
set mouse=nvi " nvi is default
" TODO: disable scrolling in edit mode
" imap <ScrollWheelUp> <Nop>
" imap <ScrollWheelDown> <Nop>
set smoothscroll
set mousescroll=ver:1,hor:1 " the same as C-e/y
set sidescrolloff=3
set nowrap
set number
set cursorline
" set list
" set listchars+=trail:_ " TODO: little bit too much for newline expanded tab

" dialogs
set confirm
set shortmess=oOTIc

" tabs, but spaces
set smartindent
set smarttab
set expandtab
set tabstop=4
set shiftwidth=4

" swiper
" TODO: show the preview window
set incsearch
set inccommand=split
set hlsearch
set ignorecase
set smartcase
set shortmess+=s
" C-g/t for next/prev match when search pattern edit is active

set showtabline=2
set laststatus=2
set statusline=%F
" set statusline+=%#LineNr#
set statusline+=%{StatuslineGit()}

set noshowmode " cursor indicates mode
set ruler
set wildmenu
set wildchar=<Tab>
set wildoptions=pum " TODO: fuzzy works for :set wld but not for :h wld

set splitbelow
set splitright

set nobackup
set nowritebackup
set noswapfile

set novisualbell

if !has('nvim')
    " TODO: keep all vanilla vim options here or save them into separate file
endif

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
nnoremap <C-s> :write<CR>
imap <C-s> <Esc>/

nmap <C-f> :FZF<CR>

" jump around wrapped text
nnoremap <silent> j gj
nnoremap <silent> k gk
" nnoremap j :silent! normal! gj<CR>
" nnoremap k :silent! normal! gk<CR>

" copy to system clipboard (default mapping on Windows is redo)
" TODO: yY should acts like 'copy the whole line into pasteboard'
" state('o') operator pending is can be used for this catch
nnoremap Y "+y
vnoremap Y "+y
nnoremap D "+d
vnoremap D "+d

" nnoremap <unique> t :echo 't is free'<CR>

noremap <silent> <Esc> :nohlsearch<CR><Esc>
" :let @/ = "" " to clear the last used search pattern

" help pages have useful map `gO` for table of contents

" Window management
" TODO: Num C-S-6 switches to buffer #Num
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-w>t :tabnew<CR>


" Project-related (git, find path)
set wildignore+=*/.git/*

" https://github.com/echasnovski/mini.nvim/issues/5
" https://github.com/neovim/neovim/pull/20633
set complete-=t
set shortmess+=C

set autochdir

" TODO:
" - [ ] netrw should set Terminal title
" FIXME:
" - [ ] sometimes using :Explore reports :pwd to one level above \
"   and therefore Enter to edit creates new empty buffer for file under cursor
let g:netrw_keepdir = 0

" TODO: doesnt works but as idea for future impl
" autocmd BufReadPost * lua require("config.bufferline_setup").close_empty_unnamed_buffers()
" autocmd VimLeavePre * if s:isActiveSession() && s:saveSession()

" https://neovim.io/doc/user/russian.html#russian.txt
set langmap="ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz"

autocmd TermOpen * setlocal nonumber | startinsert
" TODO: echo works in terminal but not in nvim
autocmd VimLeave * silent !echo -ne "\e[3 q"

" persist folds/view for markdown files
augroup MarkdownView
    autocmd!
    autocmd BufWinLeave *.md mkview
    autocmd BufWinEnter *.md silent! loadview
augroup END


lua require('my')

" lua require('haunt')
" luafile 'haunt'
" command! Haunt lua haunt()

" TODO:
" tune :h WORD for prog-mode
" plugin idea: C-Enter to complete all unclosed }).
" clean previous message after next move or so


runtime packages.vim

colorscheme alh " jellybeans
" colorscheme jellybeans
" lua require("nvim-tree").setup()
runtime lua/init.lua
runtime lua/git.lua
runtime lua/leader.lua

