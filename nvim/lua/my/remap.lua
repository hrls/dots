-- https://neovim.io/doc/user/intro.html#keycodes

-- nmap 0 looks like free binding

-- local normal = { 'Q' = 'q', 'q' = dwim_window_exit }

vim.keymap.set('n', 'Q', 'q')

vim.keymap.set('n', 'q', function()
    -- window first
    -- then tab (but last window kill should close tab itself)
    -- then buffer
    vim.api.nvim_feedkeys(vim.keycode('<C-w>q'), 'n', false)
end, { expr = true })



-- insert mode 's-enter' make newline above 'C-o s-O'
-- vim.keymap.set('i', '<S-CR>', '<Esc>O')

-- :help menu
--
-- https://vi.stackexchange.com/questions/42235/lowercase-user-commands
