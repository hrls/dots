function git_info()
    -- TODO: is it possible to rev-parse root of the project and ref in one run
    local p = vim.system({'git', 'rev-parse', '--abbrev-ref', 'HEAD'}):wait()
    if p.code ~= 0 then return {} end
    local refname = string.sub(p.stdout, 0, string.len(p.stdout) - 1)
    local root = vim.fs.root(0, '.git') -- 0 means current buffer
    return { refname = refname, root = root }
end

local function statusline()
    local gi = git_info()
    local root = gi.root -- TODO: de-normalize this path to show ~
    local refname = gi.refname
    
    return string.format(
        '%s %s %s',
        root, refname, '%F'
    )
end


vim.opt.statusline = statusline()


