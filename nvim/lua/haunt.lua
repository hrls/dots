local vim = vim

local Haunt = {}

function current_line_id()
    -- TODO
    local linenr = vim.api.nvim_win_get_cursor(0)[1]
    local curline = vim.api.nvim_buf_get_lines(0, linenr - 1, linenr, false)[1]
end

function find_buffer(name)
    local bufs = vim.api.nvim_list_bufs()
    for _, buf in ipairs(bufs) do
        print(vim.api.nvim_buf_get_name(buf))
        if vim.api.nvim_buf_get_name(buf) == name
        then return buf
        end
    end
    return nil
end

function create_buffer(name)
    local listed = false
    local scratch = true -- throwaway, nomodified, nomodeline
    local buf = vim.api.nvim_create_buf(listed, scratch)
    
    vim.api.nvim_buf_set_name(buf, name)
    return buf
end

function get_buffer(name)
    local buf = find_buffer(name)
    if buf then
        vim.api.nvim_buf_set_option(buf, 'modifiable', true)
        vim.api.nvim_buf_set_lines(buf, 0, -1, true, {})
        return buf
    else
        return create_buffer(name)
    end
end

function set_lines(buf, content)
    local line_to_id = {}
    local i = 1
    -- for id in pairs(content) do line_to_id[i] = id i = i + 1 end
    vim.api.nvim_buf_set_var(buf, 'line_to_id', line_to_id)
    -- NOTE: activating existend buffer keeps cursor position
    vim.api.nvim_buf_set_lines(buf, 0, -1, true, content)
    vim.api.nvim_buf_set_option(buf, 'modifiable', false)
    return buf
end

function init_lines()
    local data = exdata()
    local buf = get_buffer('/p/lines')

    set_lines(buf, content)

    vim.cmd('new')
    vim.api.nvim_set_current_buf(buf)
end

function get_lines()
    -- Get processs
    local handle = io.popen('ps ax')
    local result = handle:read('*a')
    handle:close()

    -- Split the result into lines
    local lines = {}
    for s in result:gmatch('[^\r\n]+') do
        table.insert(lines, s)
    end

    return lines
end

function haunt()
    local lines = get_lines()
    local buf = get_buffer('/phantom/haunt')
    set_lines(buf, lines)

    vim.cmd('new')
    vim.api.nvim_set_current_buf(buf)

    -- Bindings
    -- [i]nspect, [r]efresh, [s]end
    --
    -- vim.api.nvim_buf_set_keymap(bufnr, 'n', 'd', ':lua Haunt.kill()<CR>', { noremap = true, silent = true })
end

-- function Haunt.kill()
    -- local line = vim.api.nvim_get_current_line()
    -- os.execute("psinfo " .. line)
    -- vim.api.nvim_command("delete")
-- end

-- print("Haunt loaded")

-- return Haunt
