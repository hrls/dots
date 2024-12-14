function! GitRef()
    return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction

function! GitRoot() 
    let l:git_root = system("basename $(git rev-parse --show-toplevel)")
    if v:shell_error == 0
        echomsg 'git root:' .. l:git_root
        return l:git_root
    else
        " echohl WarningMsg | echo "Don't panic!" | echohl None
        throw 'git root unavailable'
    endif
endfunction

function! StatuslineGit()
    let l:refname = GitRef()
    " TODO:
    " - [ ] dim the title
    " - [ ] make this local to window
    return strlen(l:refname) > 0 ? ' git:'.l:refname.' ' : ''
endfunction
