
vim.g.mapleader = 's'
vim.keymap.set('n', 's', '<nop>')

local function leader(key, action)
   vim.keymap.set('n', '<leader>'..key, action)
end

leader('s', vim.cmd.write)
leader('j', vim.cmd.Oil)

leader('f', function() vim.cmd.FZF(git_info().root) end, {expr = true})
