-- TODO: make it a module

function git_info()
  -- TODO: is it possible to rev-parse root of the project and ref in one run
  local p = vim.system({ 'git', 'rev-parse', '--abbrev-ref', 'HEAD' }):wait()
  if p.code ~= 0 then return {} end
  local ref = string.sub(p.stdout, 0, string.len(p.stdout) - 1)
  local root = vim.fs.root(0, '.git') -- 0 means current buffer
  return { ref = ref, root = root }
end

function root()
  return git_info().root
end

function ref()
  return git_info().ref
end


-- [ ] should it be buffer local command or not?
vim.api.nvim_create_user_command(
  'Cdr',
  function()
    local root = git_info().root
    if root then
      vim.api.nvim_set_current_dir(root)
      print(root)
    else
      print('cwd remains untouched')
    end
  end,
  { desc = 'Set current dir to Git root' })

local function statusline()
  local gi = git_info()
  local root = gi.root or ''  -- TODO: de-normalize this path to show ~
  local refname = gi.refname or ''

  -- TODO: symbol under cursor
  return string.format('%s %s %s', '%F', '%M', '%S')
end

vim.opt.statusline = statusline()
