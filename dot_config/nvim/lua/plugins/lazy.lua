return {
  -- UI and Appearance
  {
    'gbprod/nord.nvim',
    priority = 1000,
    opts = {},
    config = function()
      require('nord').setup({
        transparent = true,
        borders = true,
        italic = false,
      })

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
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
      "JoosepAlviste/nvim-ts-context-commentstring",
    },
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
