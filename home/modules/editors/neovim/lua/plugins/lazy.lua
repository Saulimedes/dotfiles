return {
  -- UI and Appearance
  -- Colorscheme managed by Stylix (base16)
  {
    'RRethy/nvim-base16',
    priority = 1000,
    config = function()
      -- Load Stylix-generated colorscheme if available
      local stylix_theme = vim.fn.expand("~/.config/nvim/colors/base16-stylix.lua")
      if vim.fn.filereadable(stylix_theme) == 1 then
        vim.cmd("colorscheme base16-stylix")
      else
        -- Fallback to nord-inspired base16 if Stylix not available
        vim.cmd("colorscheme base16-nord")
      end

      -- Transparent background
      vim.cmd([[
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
        "lewis6991/gitsigns.nvim"
    },
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
    lazy = false,
    branch = 'v3.x',
    priority = 900,
    dependencies = {
      {'neovim/nvim-lspconfig'},
      {'williamboman/mason.nvim', build = ':MasonUpdate'},
      {'williamboman/mason-lspconfig.nvim'},
      {'WhoIsSethDaniel/mason-tool-installer.nvim'},
      {'b0o/schemastore.nvim'},
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
    dependencies = {
      "JoosepAlviste/nvim-ts-context-commentstring",
    },
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
