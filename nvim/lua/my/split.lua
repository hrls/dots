-- split2: split the window in two at the cursor without moving the screen:
-- the upper window keeps its original view and ends at cursor line - 2,
-- line cursor - 1 hides under the statusline bar, the lower active window
-- starts at the cursor line — every visible line keeps its screen row.

local function split2()
    local lnum = vim.api.nvim_win_get_cursor(0)[1]
    if lnum == 1 then
        vim.notify('split2: no lines above cursor', vim.log.levels.WARN)
        return
    end

    local win = vim.api.nvim_get_current_win()
    -- screen rows above the cursor, minus one for the upper statusline
    local height = math.max(vim.fn.winline() - 2, 1)
    height = math.min(height, math.max(lnum - 2, 1))

    -- zt honors scrolloff, drop it while placing the views
    local scrolloff = vim.o.scrolloff
    vim.o.scrolloff = 0

    vim.cmd('aboveleft ' .. height .. 'split')

    -- upper: keep the original view, so lnum - 1 ends up hidden under the
    -- statusline bar; park the cursor mid-view so scrolloff doesn't shift
    -- the view when the window is entered
    local top = math.max(lnum - height - 1, 1)
    vim.fn.winrestview({ topline = top, lnum = top + math.floor((height - 1) / 2), col = 0 })

    -- lower: cursor line at the very top
    vim.api.nvim_set_current_win(win)
    vim.cmd('normal! zt')

    vim.o.scrolloff = scrolloff
end

vim.api.nvim_create_user_command('Split2', split2, {})

-- shadows the stock :split; plain split stays on <C-w>S / <C-w><C-s>
vim.keymap.set('n', '<C-w>s', split2, { desc = 'split at cursor line' })

return { split2 = split2 }
