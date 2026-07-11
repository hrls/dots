require('oil').setup({
   default_file_explorer = true,
   columns = { 'size' },
   view_options = {
      show_hidden = true,
   },
   keymaps = {
      ['q'] = { 'actions.close', mode = 'n' },
      ['u'] = 'actions.parent',
      ['<C-r>'] = 'actions.refresh',
      ['cd'] = { 'actions.cd', opts = { scope = 'win' } },
   },
})
