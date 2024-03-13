local lsp = require('lsp-zero')
lsp.preset('recommended')

lsp.on_attach(function(client, bufnr)
  lsp.default_keymaps({buffer = bufnr})
  client.server_capabilities.semanticTokensProvider = nil
end)

lsp.ensure_installed({
  'ansiblels',
  'docker_compose_language_service',
  'lua_ls',
  'bashls',
  'dockerls',
  'terraformls',
  'pyright',
  'marksman',
  'yamlls'
})


local cmp = require('cmp')
local cmp_select = {behavior = cmp.SelectBehavior.Select}
local cmp_mappings = lsp.defaults.cmp_mappings({
  ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
  ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
  ['<C-y>'] = cmp.mapping.confirm({ select = true }),
  ["<cr>"] = cmp.mapping.confirm({ select = false }),
  ["<C-Space>"] = cmp.mapping.complete(),
})

cmp_mappings['<Tab>'] = nil
cmp_mappings['<S-Tab>'] = nil

--vim.lsp.set_log_level("debug")

lsp.setup_nvim_cmp({
  mapping = cmp_mappings,
  sources = {
    {name = 'path'},
    {name = 'tmux'},
    {name = 'cmp_git'},
    {name = 'cmdline'},
    {name = 'buffer', keyword_length = 3},
    {name = 'luasnip', keyword_length = 2},
  }
})

-- (Optional) Configure lua language server for neovim
lsp.nvim_workspace()

lsp.setup {
  incremental_sync = true
}

