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
    -- statusline bar; park the cursor on the lowest visible line, with
    -- window-local scrolloff zeroed (it overrides the global padding, they
    -- don't stack) so entering the window doesn't shift the view
    local top = math.max(lnum - height - 1, 1)
    vim.wo.scrolloff = 0
    vim.fn.winrestview({ topline = top, lnum = top + height - 1, col = 0 })

    -- lower: cursor line at the very top
    vim.api.nvim_set_current_win(win)
    vim.cmd('normal! zt')

    vim.o.scrolloff = scrolloff
end

vim.api.nvim_create_user_command('Split2', split2, {})

-- shadows the stock :split; plain split stays on <C-w>S / <C-w><C-s>
vim.keymap.set('n', '<C-w>s', split2, { desc = 'split at cursor line' })

-- attach2: bond the same-width window above or below showing the same
-- buffer, sliding its view to continue this window's content with Split2
-- geometry (one line hidden under the statusline bar between them). The
-- follower keeps re-aligning while this window scrolls; the bond releases
-- itself when either window closes or shows another buffer. Calling
-- attach2 again from the same window releases the bond by hand.

local bonds = {} -- leader win id -> { follower, above, buf, group, so }

local function detach(leader)
    local bond = bonds[leader]
    if not bond then return end
    bonds[leader] = nil
    pcall(vim.api.nvim_del_augroup_by_id, bond.group)
    if vim.api.nvim_win_is_valid(bond.follower) then
        vim.wo[bond.follower].scrolloff = bond.so
    end
    vim.notify('attach2: released', vim.log.levels.INFO)
end

local function realign(leader)
    local bond = bonds[leader]
    if not bond then return end
    if not (vim.api.nvim_win_is_valid(leader)
            and vim.api.nvim_win_is_valid(bond.follower)
            and vim.api.nvim_win_get_buf(leader) == bond.buf
            and vim.api.nvim_win_get_buf(bond.follower) == bond.buf) then
        detach(leader)
        return
    end

    local last = vim.api.nvim_buf_line_count(bond.buf)
    local view
    if bond.above then
        -- follower ends at leader topline - 2, one line under the bar
        local ltop = vim.api.nvim_win_call(leader, function() return vim.fn.line('w0') end)
        local height = vim.api.nvim_win_get_height(bond.follower)
        local top = math.max(ltop - height - 1, 1)
        view = { topline = top, lnum = math.min(top + height - 1, last), col = 0 }
    else
        -- follower starts at leader bottom line + 2
        local lbot = vim.api.nvim_win_call(leader, function() return vim.fn.line('w$') end)
        local top = math.min(lbot + 2, last)
        view = { topline = top, lnum = top, col = 0 }
    end

    vim.api.nvim_win_call(bond.follower, function()
        if vim.fn.line('w0') ~= view.topline or vim.fn.line('.') ~= view.lnum then
            vim.fn.winrestview(view)
        end
    end)
end

-- window right above/below with the same width and buffer as win
local function neighbor(win, dir)
    local other = vim.fn.win_getid(vim.fn.winnr(dir))
    if other == 0 or other == win then return nil end
    if vim.fn.win_screenpos(other)[2] ~= vim.fn.win_screenpos(win)[2]
        or vim.api.nvim_win_get_width(other) ~= vim.api.nvim_win_get_width(win)
        or vim.api.nvim_win_get_buf(other) ~= vim.api.nvim_win_get_buf(win) then
        return nil
    end
    return other
end

local function attach2()
    local leader = vim.api.nvim_get_current_win()
    if bonds[leader] then
        detach(leader)
        return
    end

    local follower, above = neighbor(leader, 'k'), true
    if not follower then
        follower, above = neighbor(leader, 'j'), false
    end
    if not follower then
        vim.notify('attach2: no same-width window with this buffer above or below', vim.log.levels.WARN)
        return
    end

    local group = vim.api.nvim_create_augroup('attach2_' .. leader, {})
    bonds[leader] = {
        follower = follower,
        above = above,
        buf = vim.api.nvim_win_get_buf(leader),
        group = group,
        so = vim.api.nvim_get_option_value('scrolloff', { win = follower, scope = 'local' }),
    }
    vim.wo[follower].scrolloff = 0

    vim.api.nvim_create_autocmd('WinScrolled', {
        group = group,
        callback = function() realign(leader) end,
    })
    vim.api.nvim_create_autocmd('WinClosed', {
        group = group,
        pattern = { tostring(leader), tostring(follower) },
        callback = function() detach(leader) end,
    })

    realign(leader)
    vim.notify('attach2: bonded ' .. (above and 'upper' or 'lower') .. ' window', vim.log.levels.INFO)
end

vim.api.nvim_create_user_command('Attach2', attach2, {})

return { split2 = split2, attach2 = attach2 }
