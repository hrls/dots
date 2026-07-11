" TODO:
" [ ] clear all errors on buffer save until linter returns to prevent outdated errors when linter is active
" [ ] show preview window when linting process is active

let g:ale_disable_lsp = 1

nmap > :ALENext<CR>
nmap < :ALEPrevious<CR>
