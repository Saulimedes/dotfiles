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

-- Setup mason with automatic installation
require('mason').setup({
  ui = {
    border = "rounded",
    icons = {
      package_installed = "✓",
      package_pending = "➜",
      package_uninstalled = "✗"
    }
  }
})

-- Setup mason-lspconfig with automatic server installation
require('mason-lspconfig').setup({
  ensure_installed = {
    'ansiblels',
    'bashls',
    'cssls',
    'docker_compose_language_service',
    'dockerls',
    'html',
    'jsonls',
    'lua_ls',
    'marksman',
    'pyright',
    'rust_analyzer',
    'terraformls',
    'ts_ls',
    'yamlls'
  },
  automatic_installation = true,
  handlers = {
    -- Default handler for all servers
    lsp_zero.default_setup,

    -- Specific lua_ls configuration
    lua_ls = function()
      require('lspconfig').lua_ls.setup({
        settings = {
          Lua = {
            runtime = {
              version = 'LuaJIT',
            },
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
      })
    end,

    -- Ansible configuration (only for ansible files, not all yaml)
    ansiblels = function()
      require('lspconfig').ansiblels.setup({
        settings = {
          ansible = {
            ansible = {
              path = "ansible"
            },
            executionEnvironment = {
              enabled = false
            },
            python = {
              interpreterPath = "python3"
            },
            validation = {
              enabled = true,
              lint = {
                enabled = true,
                path = "ansible-lint"
              }
            }
          }
        },
        filetypes = { "yaml.ansible" }
      })
    end,

    -- YAML configuration
    yamlls = function()
      require('lspconfig').yamlls.setup({
        settings = {
          yaml = {
            schemas = {
              kubernetes = "/*.yaml",
              ["http://json.schemastore.org/github-workflow"] = ".github/workflows/*",
              ["http://json.schemastore.org/github-action"] = ".github/action.{yml,yaml}",
              ["http://json.schemastore.org/ansible-stable-2.9"] = "roles/tasks/*.{yml,yaml}",
              ["http://json.schemastore.org/prettierrc"] = ".prettierrc.{yml,yaml}",
              ["http://json.schemastore.org/kustomization"] = "kustomization.{yml,yaml}",
              ["http://json.schemastore.org/chart"] = "Chart.{yml,yaml}",
              ["https://json.schemastore.org/dependabot-v2"] = ".github/dependabot.{yml,yaml}",
              ["https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json"] = "*docker-compose*.{yml,yaml}",
            },
            format = { enabled = true },
            validate = true,
            completion = true,
            hover = true,
          }
        }
      })
    end,

    -- Python configuration
    pyright = function()
      require('lspconfig').pyright.setup({
        settings = {
          python = {
            analysis = {
              autoSearchPaths = true,
              useLibraryCodeForTypes = true,
              diagnosticMode = "workspace"
            }
          }
        }
      })
    end,

    -- TypeScript/JavaScript configuration
    ts_ls = function()
      require('lspconfig').ts_ls.setup({
        settings = {
          typescript = {
            inlayHints = {
              includeInlayParameterNameHints = 'all',
              includeInlayParameterNameHintsWhenArgumentMatchesName = false,
              includeInlayFunctionParameterTypeHints = true,
              includeInlayVariableTypeHints = true,
              includeInlayPropertyDeclarationTypeHints = true,
              includeInlayFunctionLikeReturnTypeHints = true,
              includeInlayEnumMemberValueHints = true,
            }
          },
          javascript = {
            inlayHints = {
              includeInlayParameterNameHints = 'all',
              includeInlayParameterNameHintsWhenArgumentMatchesName = false,
              includeInlayFunctionParameterTypeHints = true,
              includeInlayVariableTypeHints = true,
              includeInlayPropertyDeclarationTypeHints = true,
              includeInlayFunctionLikeReturnTypeHints = true,
              includeInlayEnumMemberValueHints = true,
            }
          }
        }
      })
    end,

    -- JSON configuration
    jsonls = function()
      require('lspconfig').jsonls.setup({
        settings = {
          json = {
            schemas = require('schemastore').json.schemas(),
            validate = { enable = true },
          }
        }
      })
    end,

    -- HTML configuration
    html = function()
      require('lspconfig').html.setup({
        filetypes = { "html", "htmldjango" }
      })
    end,

    -- CSS configuration
    cssls = function()
      require('lspconfig').cssls.setup({
        settings = {
          css = {
            validate = true,
            lint = {
              unknownAtRules = "ignore"
            }
          },
          scss = {
            validate = true,
            lint = {
              unknownAtRules = "ignore"
            }
          },
          less = {
            validate = true,
            lint = {
              unknownAtRules = "ignore"
            }
          }
        }
      })
    end,

    -- Rust configuration
    rust_analyzer = function()
      require('lspconfig').rust_analyzer.setup({
        settings = {
          ['rust-analyzer'] = {
            cargo = {
              allFeatures = true,
            },
            checkOnSave = {
              command = "clippy"
            },
          }
        }
      })
    end,
  }
})

-- Mason Tool Installer for non-LSP tools (formatters, linters, DAPs)
require('mason-tool-installer').setup({
  ensure_installed = {
    'ansible-lint',
    'prettier',
    'stylua',
    'shellcheck',
    'shfmt',
    'tflint',
    'yamllint',
    'black',
    'isort',
    'pylint',
  },
  auto_update = true,
  run_on_start = true,
})


-- nvim-cmp setup
local cmp = require('cmp')
local luasnip = require('luasnip')

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

