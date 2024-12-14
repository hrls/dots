" :Inspect
" :so $VIMRUNTIME/syntax/hitest.vim

let g:jellybeans_use_term_italics = 1

colorscheme jellybeans


highlight clear CursorLine
highlight Delimiter ctermfg=darkgrey

" Inactive windows
highlight NormalNC ctermfg=grey

" WinEnter / WinLeave

" autoload WinEnter :set
" winhighlight=Normal:Inactive,Function:Normal,Keyword:InactiveF

augroup InactiveWindow
    autocmd WinEnter * set winhighlight&
    autocmd WinLeave * set winhighlight=Normal:Delimiter,Type:Normal,Statement:Normal
augroup END
