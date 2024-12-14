-- ttt - type to tab

-- :h completeopt omnifunc completefunc complete-functions
-- :h ins-completion-menu
-- :h :map-<expr>

vim.opt.completeopt = { 'menu', 'menuone', 'noselect', 'popup' }

-- infix hints are out of scope
local function inline_hint()
    local pos = vim.api.nvim_win_get_cursor(0)

    -- local buf = vim.api.nvim_get_current_buf()
    local ns = vim.api.nvim_create_namespace('ttt')

    local mark = vim.api.nvim_buf_set_extmark(0, ns, 0, 0, {})
    
    -- local all = api.nvim_buf_get_extmarks(0, ns, 0, -1, {})


    -- Deprecated
    -- vim.api.nvim_buf_set_virtual_text(buf, ns, 0, {{'Just', 'Comment'}}, {})

    -- virt_text_pos = 'overlay'
    -- virt_text_pos = 'eol'
    -- local opts = { id = 1, end_line = 10, virt_text = {{"just", "Delimiter"}} }
    -- local mark_id = vim.api.nvim_buf_set_extmark(buf, ns, line, column, opts)
end

vim.keymap.set('i', '<Tab>', function()
    if vim.fn.pumvisible() == 1 then
        return '<C-n>'
    end
    return '<Tab>'
end, { expr = true })

--	inoremap <expr> . MayComplete()
--	func MayComplete()
--	    if (can complete)
--	      return ".\<C-X>\<C-O>"
--	    endif
--	    return '.'
--	endfunc


-- TODO: make it buffer local and init with delimiters for filetype (maybe from grammar)
local triggers = {'.'}
vim.api.nvim_create_autocmd("InsertCharPre", {
    buffer = vim.api.nvim_get_current_buf(),
    callback = function()
        if vim.fn.pumvisible() == 1 or vim.fn.state('m') == 'm' then
            return
        end
        local char = vim.v.char
        if vim.list_contains(triggers, char) then
            local key = vim.keycode("<C-x><C-n><C-p>")
            vim.api.nvim_feedkeys(key, 'm', false)
        end
    end
})

