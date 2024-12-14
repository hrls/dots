" ftplugin/rust.vim
" let g:rustfmt_options = '--edition 2021'

let b:rustfmt_autosave_if_config_present = 1
let b:rust_fold = 1

" :h treesitter-parsers
" lua vim.treesitter.language.add('rust', { path = '/path/to/treesitter/rust.so' })

let b:ale_linters = ['cargo']

setlocal tags=! " disable tags

