" TODO:
" [ ] - red/blue looks weird, try purple for keywords, bold dim for literals
" runtime colors/peachpuff.vim

source $VIMRUNTIME/colors/vim.lua
set background=light
let g:colors_name = 'alh'

hi Normal ctermbg=231 ctermfg=232 cterm=NONE
hi Type  ctermfg=33 cterm=bold
hi Identifier ctermfg=232 " cterm=bold
hi Statement ctermfg=105 " for if
hi Constant ctermfg=196 cterm=italic " literals

hi clear CursorLine

hi Quest ctermfg=1
hi link MoreMsg Quest

hi Special ctermfg=242
hi Comment ctermfg=242
hi PreProc ctermfg=26
hi Title ctermfg=black cterm=bold

hi LineNr ctermfg=250
hi CursorLineNr ctermfg=232 cterm=bold,nocombine

hi TabLine cterm=NONE

hi Directory ctermfg=33 cterm=bold

" hi @comment.warning ctermfg=64
hi DiagnosticVirtualTextWarn ctermbg=11 ctermfg=black

" hi clear CursorLine
" hi clear TabLine

" hi PreProc ctermfg=25

" hi Type ctermfg=232 cterm=NONE " 91 53

" hi Identifier ctermfg=232 " cterm=NONE

" hi Comment ctermfg=239 cterm=italic


" hi rustFuncName cterm=italic
" hi rustFuncCall cterm=italic


