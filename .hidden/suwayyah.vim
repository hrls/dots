function! OmniTab()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
    " echo "cl: " . cl
    " check if cl match any char except tabs or spaces
    " then autocomplete
    " else tab
    " return "\<tab>"
endfunction

function! BsSTab()
    return "\<tab>"
endfunction

" s-tab for navigate back on men
" s-tab on empty should works as backspace
inoremap <tab> <c-r>=OmniTab()<cr>
inoremap <s-tab> <c-r>=BsSTab()<cr>
