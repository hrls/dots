function! OmniTab()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
    " expand fb to FooBar (? tagbar ext)
endfunction

function! BsSTab()
    return "\<tab>"
endfunction

" s-tab for navigate back on men
" s-tab on empty should works as backspace
inoremap <silent> <tab> <c-r>=OmniTab()<cr>
inoremap <silent> <s-tab> <c-r>=BsSTab()<cr>
