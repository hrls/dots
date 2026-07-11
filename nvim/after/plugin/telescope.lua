--  TODO:
--  - [ ] :Telescope bindings - new picker for search over bindings
--  - [ ] :Telescope vim_options should display :help 'option' as preview

local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')

local kill_buffer = function(prompt_bufnr)
  local selected = action_state.get_selected_entry()
  actions.close(prompt_bufnr)
  vim.api.nvim_buf_delete(selected.bufnr, { force = true })
end

require('telescope').setup{
  defaults = {
    layout_strategy = 'vertical',
    layout_config = { height = 0.7 },
    mappings = {
      i = { ['<C-h>'] = 'which_key' },
      --[[
      pickers  = {
        buffers = {
          mappings = {
            i = { ['<C-k>'] = 'kill_buffer' }
          }
        }
      }
      ]]
    }
  },
  pickers = {
    buffers = { theme = 'dropdown' },
    find_files = {
      layout_strategy = 'vertical',
      layout_config = { preview_height = 0.2 },
    },
    git_files = { 
      layout_strategy = 'vertical',
      layout_config = { preview_height = 0.2 },
    },
    help_tags = {
      layout_strategy = 'horizontal',
      layout_config = { width = 0.9, preview_width = 0.5 },
    },
  },
  extensions = {
  }
}

local builtin = require('telescope.builtin')

-- TODO: project related buffers call git_files instead of find_files
-- vim.keymap.set('n', '<leader>f',  builtin.find_files, { desc = 'find files' })
vim.keymap.set('n', '<leader>r',  builtin.live_grep,  { desc = 'live grep' })
vim.keymap.set('n', '<leader>e',  builtin.buffers,    { desc = 'buffers' })
vim.keymap.set('n', '<leader>ht', builtin.help_tags,  { desc = 'help tags' })

