function! OmniTab()
    let cl = getline('.')
    echo "cl: " . cl
    " check if cl match any char except tabs or spaces
    " then autocomplete
    " else tab
    return "\<tab>"
endfunction

inoremap <Tab> <c-r>=OmniTab()<CR>
