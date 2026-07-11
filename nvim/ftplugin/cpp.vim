" C++
" TODO: highlight `<>`

autocmd BufRead $SDK_ROOT/* set readonly shiftwidth=8

" it works, but inconsistently, use Tab instead
" autocmd FileType cpp :iabbrev <buffer> a auto


" https://github.com/dense-analysis/ale?tab=readme-ov-file#how-can-i-configure-my-c-or-c-project
if has("ale")
    let b:ale_fixers = { 'cpp': ['make'] }
endif

let g:ale_cpp_cc_options = '-std=c++23 -Wall'

" TODO: disable reindent using labels like 'public:' ':h indentkeys'
