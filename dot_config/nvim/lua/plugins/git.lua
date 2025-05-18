-- Git integration setup for gitsigns.nvim
require('gitsigns').setup({
  signs = {
    add          = {text = '▎'},
    change       = {text = '▎'},
    delete       = {text = '_'},
    topdelete    = {text = '‾'},
    changedelete = {text = '~'},
    untracked    = {text = '▎'},
  },
  signs_staged = {
    add          = { text = '┃' },
    change       = { text = '┃' },
    delete       = { text = '_' },
    topdelete    = { text = '‾' },
    changedelete = { text = '~' },
    untracked    = { text = '┆' },
  },
  current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
  signcolumn = true,  -- Toggle with `:Gitsigns toggle_signs`
  numhl      = false, -- Toggle with `:Gitsigns toggle_numhl`
  linehl     = false, -- Toggle with `:Gitsigns toggle_linehl`
  max_file_length = 40000,
  watch_gitdir = {
    interval = 1000,
    follow_files = true
  },
  attach_to_untracked = true,
  update_debounce = 100,
  status_formatter = nil, -- Use default
})

-- Set highlights to resolve deprecations
vim.api.nvim_set_hl(0, 'GitSignsAdd', { link = 'GitGutterAdd' })
vim.api.nvim_set_hl(0, 'GitSignsChange', { link = 'GitGutterChange' })
vim.api.nvim_set_hl(0, 'GitSignsDelete', { link = 'GitGutterDelete' })
vim.api.nvim_set_hl(0, 'GitSignsChangedelete', { link = 'GitGutterChange' })

-- Optional: if you are using line highlights or numhl, link them as well
vim.api.nvim_set_hl(0, 'GitSignsAddNr', { link = 'GitGutterAdd' })
vim.api.nvim_set_hl(0, 'GitSignsChangeNr', { link = 'GitGutterChange' })
vim.api.nvim_set_hl(0, 'GitSignsDeleteNr', { link = 'GitGutterDelete' })
vim.api.nvim_set_hl(0, 'GitSignsAddLn', { link = 'GitGutterAdd' })
vim.api.nvim_set_hl(0, 'GitSignsChangeLn', { link = 'GitGutterChange' })
vim.api.nvim_set_hl(0, 'GitSignsDeleteLn', { link = 'GitGutterDelete' })
vim.api.nvim_set_hl(0, 'GitSignsTopdelete', { link = 'GitGutterDelete' })
