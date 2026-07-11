" ftplugin/rust.vim

" let g:rustfmt_options = '--edition 2024'
" let b:rustfmt_autosave_if_config_present = 1
let b:rust_fold = 1

" :h treesitter-parsers
lua vim.treesitter.language.add('rust', { path = '/Users/hrls/src/thirdparty/tree-sitter-rust/libtree-sitter-rust.dylib' })
" lua vim.treesitter.start()
"
" TODO:
" - [ ] if treesitter is available then last edit mark can be set to some
"   point in the tree, and then after formatting file it can be restored to
"   prevent jump to unknown area (basically eof)
"
" - [ ] :Cargo run opens read-only buffer but r key is used as default.
"   Override some bindings to make this windows usable in workflow. At least
"   restart should be there.

let b:ale_linters = ['cargo']
let g:ale_rust_cargo_use_clippy = 1
let g:ale_rust_cargo_check_tests = 1
let g:ale_rust_cargo_check_examples = 1
let g:ale_rust_cargo_check_all_targets=1
let g:rust_cargo_check_all_targets = 1
let g:rustfmt_fail_silently = 1
let b:ale_fixers = ['remove_trailing_lines']

nnoremap <buffer> <C-f> :RustFmt<CR>

" setlocal tags=! " disable tags

hi link rustFuncName Identifier
hi rustFuncName cterm=bold
