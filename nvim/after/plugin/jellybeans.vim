" :Inspect
" source $VIMRUNTIME/syntax/hitest.vim

let g:jellybeans_use_term_italics = 1

colorscheme jellybeans


highlight clear CursorLine
highlight Delimiter ctermfg=darkgrey
highlight PmenuSel ctermbg=blue

" TODO: highlight matched parens line (see :h pi_paren.txt match-parens)

" Inactive windows
highlight NormalNC ctermfg=grey

" WinEnter / WinLeave

" autoload WinEnter :set
" winhighlight=Normal:Inactive,Function:Normal,Keyword:InactiveF

augroup InactiveWindow
    autocmd WinEnter,BufEnter * set winhighlight&
    autocmd WinLeave * set winhighlight=Normal:Delimiter,Type:Normal,Statement:Normal
augroup END
