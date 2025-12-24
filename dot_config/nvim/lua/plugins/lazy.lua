return {
  -- UI and Appearance
  {
    'shaunsingh/nord.nvim',
    priority = 1000,
    config = function()
      vim.g.nord_contrast = true
      vim.g.nord_borders = false
      vim.g.nord_disable_background = true
      vim.g.nord_italic = false
      vim.g.nord_uniform_diff_background = true
      vim.g.nord_bold = false

      vim.cmd([[
        colorscheme nord
        highlight Normal guibg=NONE ctermbg=NONE
        highlight NormalFloat guibg=NONE ctermbg=NONE
      ]])
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    opts = {}
  },
  {
    'akinsho/bufferline.nvim',
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function()
      require("bufferline").setup{
        options = {
          mode = "buffers",
          left_mouse_command = "buffer %d",
          close_command = "bdelete! %d",
          always_show_bufferline =  false,
          offsets = {
            {
              filetype = "NvimTree",
              text = "File Explorer",
              text_align = "center",
              separator = true,
            }
          },
          hover = {
            enabled = true,
            delay = 150,
            reveal = {'close'}
          }
        }
      }
    end
  },
  {
    'windwp/windline.nvim',
    dependencies = {
       "nvim-tree/nvim-web-devicons",
       "f-person/git-blame.nvim",
        "lewis6991/gitsigns.nvim"
    },
  },
  {
    "f-person/git-blame.nvim",
    config = function()
      vim.g.gitblame_enabled = 1
      vim.g.gitblame_display_virtual_text = 0
      vim.g.gitblame_message_when_not_committed = ''
      vim.g.gitblame_date_format = '%m/%d'
      vim.g.gitblame_message_template = '  <date> <author> ∙ <summary> '
      -- Force git-blame to use bash instead of fish
      vim.g.gitblame_shell = '/bin/bash'
    end,
  },
  -- File Explorer and Navigation
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    },
    config = function()
      require("neo-tree").setup({
        close_if_last_window = true,
      })
    end
  },
  {
    "phaazon/hop.nvim",
    lazy = true,
  },

  -- Git Integration
  {
    "lewis6991/gitsigns.nvim",
  },
  -- Language Support and LSP
  {
    'VonHeikemen/lsp-zero.nvim',
    lazy = true,
    branch = 'v3.x',
    dependencies = {
      {'neovim/nvim-lspconfig'},
      {'williamboman/mason.nvim', build = function() pcall(vim.api.nvim_command, 'MasonUpdate') end},
      {'williamboman/mason-lspconfig.nvim'},
      {'hrsh7th/nvim-cmp'},
      {'hrsh7th/cmp-nvim-lsp'},
      {'hrsh7th/cmp-buffer'},
      {'hrsh7th/cmp-path'},
      {'saadparwaiz1/cmp_luasnip'},
      {'hrsh7th/cmp-nvim-lua'},
      {'andersevenrud/cmp-tmux'},
      {'hrsh7th/cmp-cmdline'},
      {'L3MON4D3/LuaSnip'},
      {'rafamadriz/friendly-snippets'},
    }
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      pcall(function()
        require("nvim-treesitter.configs").setup({
          ensure_installed = {"c", "python", "perl", "lua", "css", "html", "rust", "yaml", "markdown", "toml", "bash", "jq", "json", "javascript", "typescript" },
          sync_install = false,
          auto_install = true,
          highlight = {
            enable = true,
            disable = { "markdown" },
            additional_vim_regex_highlighting = false,
          },
          indent = { enable = true },
          incremental_selection = {
            enable = true,
            keymaps = {
              init_selection = "<CR>",
              node_incremental = "<CR>",
              scope_incremental = "<S-CR>",
              node_decremental = "<BS>",
            },
          },
        })
      end)
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    event = "VeryLazy",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      -- Delay loading and wrap in pcall to handle missing configs gracefully
      vim.defer_fn(function()
        pcall(function()
          require("nvim-treesitter.configs").setup({
            textobjects = {
              select = {
                enable = true,
                lookahead = true,
                keymaps = {
                  ["af"] = "@function.outer",
                  ["if"] = "@function.inner",
                  ["ac"] = "@class.outer",
                  ["ic"] = "@class.inner",
                  ["aa"] = "@parameter.outer",
                  ["ia"] = "@parameter.inner",
                },
              },
              move = {
                enable = true,
                set_jumps = true,
                goto_next_start = {
                  ["]f"] = "@function.outer",
                  ["]c"] = "@class.outer",
                },
                goto_next_end = {
                  ["]F"] = "@function.outer",
                  ["]C"] = "@class.outer",
                },
                goto_previous_start = {
                  ["[f"] = "@function.outer",
                  ["[c"] = "@class.outer",
                },
                goto_previous_end = {
                  ["[F"] = "@function.outer",
                  ["[C"] = "@class.outer",
                },
              },
            },
          })
        end)
      end, 100)
    end,
  },

  -- Utilities
  {
    "goolord/alpha-nvim",
    lazy = true,
  },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("trouble").setup({
        position = "bottom",
        icons = true,
        auto_open = false,
        auto_close = true,
        use_diagnostic_signs = true
      })
    end
  },
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("todo-comments").setup()
    end
  },
  {
    "windwp/nvim-autopairs"
  },
  {
    "taybart/b64.nvim"
  },
  {
    'numToStr/Comment.nvim',
    lazy = false,
    dependencies = {
      'JoosepAlviste/nvim-ts-context-commentstring',
    },
    config = function()
      -- Comment.nvim is now configured in ts-context.lua to use the integration
    end
  },
  {
    "kylechui/nvim-surround",
    config = function()
      require("nvim-surround").setup()
    end
  },
  {
    'nvim-telescope/telescope.nvim',
    lazy = true,
    dependencies = {
      'nvim-lua/plenary.nvim',
      'debugloop/telescope-undo.nvim'
    }
  },
  {
    "mfussenegger/nvim-dap",
  },
  {
    'akinsho/toggleterm.nvim',
    version = "*",
    config = true
  },
  {
    "jiaoshijie/undotree",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
  },
  {
    'folke/which-key.nvim',
    dependencies = 'echasnovski/mini.icons',
    event = "VeryLazy",
  },
}
