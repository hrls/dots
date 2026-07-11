" Omni Tab
" :h ins-completion
" complete_info() - get the current completion info
"
" C-e stops , C-y accepts
" :h popupmenu-keys
" :h compl-current
" :h completefunc
" :h vim.lua_omnifunc
"
" 
"
" https://neovim.io/doc/user/autocmd.html#CompleteChanged
" https://neovim.io/doc/user/insert.html#complete-items
"
" Idle detection
" https://neovim.io/doc/user/options.html#'updatetime'
" https://neovim.io/doc/user/windows.html#CursorHold-example
"
" :Pwtab - open new terminal tab with :pwd as $PWD

let s:triggers = ['.'] " TODO: re-use delimiters from filetype

" let Pchar = {-> 'ignoerr' }

function! Pchar()
    if pumvisible() == 1
        echo .
        return
    endif
    let l:c = v:char
    if index(s:triggers, c) >= 0
        feedkeys("<C-x><C-n>", 'm')
    endif
endfunction

function! Clear()
    " echo 'clear completion'
endfunction

command Abspath :let @+ = expand('%:p') " copy to pasteboard current buffer absolute path

augroup ot
    autocmd!
    autocmd InsertCharPre <buffer> :call Pchar()
    autocmd InsertLeavePre <buffer> :echo 'leave pre'
    " autocmd CursorMovedI <buffer> :call Clear()
augroup end


" autocmd BufWritePost <sfile> ++once source <sfile>

" see :h 565
function! CleverTab()
    if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
        return "\<Tab>"
    else
        return "\<C-N>"
    endif
endfunction
inoremap <Tab> <C-R>=CleverTab()<CR>

