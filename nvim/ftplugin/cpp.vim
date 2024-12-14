" C++
" TODO: highlight `<>`

autocmd BufRead $SDK_ROOT/* set readonly shiftwidth=8

" it works, but inconsistently, use Tab instead
" autocmd FileType cpp :iabbrev <buffer> a auto


if has("ale")
    let b:ale_fixers = { 'cpp': ['make'] }
endif
