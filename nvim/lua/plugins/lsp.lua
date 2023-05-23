local lsp = require('lsp-zero')
lsp.preset('recommended')

lsp.ensure_installed({
  'ansiblels',
  'bashls',
  'lua_ls',
  'ltex',
  'rust_analyzer',
  'jsonls',
  'dockerls',
  'pyright',
  'marksman',
  'yamlls'
})

lsp.automatic_installation = true,

lsp.setup_nvim_cmp({
  sources = {
    {name = 'path'},
    {name = 'tmux'},
    {name = 'nvim_lsp'},
    {name = 'cmp_git'},
    {name = 'cmdline'},
    {name = 'buffer', keyword_length = 3},
    {name = 'luasnip', keyword_length = 2},
  }
})

-- (Optional) Configure lua language server for neovim
lsp.nvim_workspace()

lsp.setup()
