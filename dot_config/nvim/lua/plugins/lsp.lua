local lsp_zero = require('lsp-zero')

-- Load default LSP-zero behavior
lsp_zero.extend_lspconfig()

-- Keymaps + client capabilities
lsp_zero.on_attach(function(client, bufnr)
  lsp_zero.default_keymaps({buffer = bufnr})

  -- Disable semantic tokens for performance
  client.server_capabilities.semanticTokensProvider = nil

  local opts = {buffer = bufnr, remap = false}
  local map = vim.keymap.set
  map("n", "gd", vim.lsp.buf.definition, opts)
  map("n", "K", vim.lsp.buf.hover, opts)
  map("n", "<leader>vws", vim.lsp.buf.workspace_symbol, opts)
  map("n", "<leader>vd", vim.diagnostic.open_float, opts)
  map("n", "[d", vim.diagnostic.goto_next, opts)
  map("n", "]d", vim.diagnostic.goto_prev, opts)
  map("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  map("n", "<leader>vrr", vim.lsp.buf.references, opts)
  map("n", "<leader>vrn", vim.lsp.buf.rename, opts)
  map("i", "<C-h>", vim.lsp.buf.signature_help, opts)
end)

-- Setup mason + LSP installations
require('mason').setup()

require('mason-lspconfig').setup({
  ensure_installed = {
    'ansiblels',
    'docker_compose_language_service',
    'lua_ls',
    'bashls',
    'dockerls',
    'terraformls',  -- Terraform language server (works with OpenTofu)
    'tflint',       -- Terraform/OpenTofu linter
    'pyright',
    'marksman',
    'yamlls'
  },
  handlers = {
    lsp_zero.default_setup,
  }
})

-- Specific lua_ls config
vim.lsp.config.lua_ls = {
  cmd = { 'lua-language-server' },
  settings = {
    Lua = {
      diagnostics = {
        globals = { 'vim' }
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false
      },
      telemetry = {
        enable = false,
      },
    }
  }
}


-- nvim-cmp setup
local cmp = require('cmp')
local luasnip = require('luasnip')
local cmp_action = require('lsp-zero').cmp_action()

cmp.setup({
  mapping = cmp.mapping.preset.insert({
    ['<C-p>']     = cmp.mapping.select_prev_item(),
    ['<C-n>']     = cmp.mapping.select_next_item(),
    ['<C-y>']     = cmp.mapping.confirm({ select = true }),
    ['<CR>']      = cmp.mapping.confirm({ select = false }),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, {"i", "s"}),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, {"i", "s"}),
  }),
  sources = cmp.config.sources({
    {name = 'nvim_lsp', priority = 1000},
    {name = 'luasnip',  priority = 750},
    {name = 'path',     priority = 500},
    {name = 'tmux',     priority = 250},
    {name = 'buffer',   keyword_length = 3, priority = 200},
  }),
  formatting = {
    fields = {'abbr', 'kind', 'menu'},
    format = function(entry, vim_item)
      vim_item.menu = ({
        nvim_lsp = "[LSP]",
        luasnip  = "[Snippet]",
        buffer   = "[Buffer]",
        path     = "[Path]",
        tmux     = "[Tmux]",
      })[entry.source.name]
      return vim_item
    end,
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
})

-- Global diagnostics style
vim.diagnostic.config({
  virtual_text = { spacing = 4, prefix = '●' },
  underline = true,
  update_in_insert = false,
  severity_sort = true,
  float = {
    focusable = true,
    style = 'minimal',
    border = 'rounded',
    source = 'always',
    header = '',
    prefix = '',
  },
})

