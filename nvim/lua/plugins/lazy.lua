return {
   -- Alpha
    {
        "goolord/alpha-nvim",
        lazy = true,
    },
    -- Autopairs
    {
        "windwp/nvim-autopairs"
    },
    -- Bufferline 
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
    -- Colorscheme
    {  'jeffkreeftmeijer/vim-dim',
        priority = 1000,
        config = function()
          vim.cmd([[colorscheme dim]])
          vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
          vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
        end
    },
    -- Hop (Better Navigation)
    {
        "phaazon/hop.nvim",
        lazy = true,
    },
    -- Surround
    {
        "kylechui/nvim-surround",
        config = function()
            require("nvim-surround").setup({
                -- Configuration here, or leave empty to use defaults
            })
        end
    }, 
    -- Telescoppe
    {
        'nvim-telescope/telescope.nvim',
        lazy = true,
        dependencies = {
            {'nvim-lua/plenary.nvim'},
        }
    },
    -- nvimtree
    {
        'nvim-tree/nvim-tree.lua',
        lazy = true,
        dependencies = {
            'nvim-tree/nvim-web-devicons',
        },
    },
    -- Indentation Highlighting
    {
        "lukas-reineke/indent-blankline.nvim",
    },
    -- Rainbow Highlighting
    {
        "HiPhish/nvim-ts-rainbow2",
    },
    -- lualine
    {
        'nvim-lualine/lualine.nvim',
        dependencies = 'nvim-tree/nvim-web-devicons',
        config = function()
          require('lualine').setup {
            options = {
              theme = 'auto',
              disabled_filetypes = { 'lazy', 'NVimTree' },
              component_separators = '',
              section_separators = '',
            }
          }
        end
    },
    -- DAP
    {
      "mfussenegger/nvim-dap",
    },
    -- Git Integration
    {
        "lewis6991/gitsigns.nvim",
    },
    -- Language Support
    {
        'VonHeikemen/lsp-zero.nvim',
        lazy = true,
        branch = 'v1.x',
        dependencies = {
            -- LSP Support
            {'neovim/nvim-lspconfig'},             -- Required
            {'williamboman/mason.nvim'},           -- Optional
            {'williamboman/mason-lspconfig.nvim'}, -- Optional

            -- Autocompletion
            {'hrsh7th/nvim-cmp'},         -- Required
            {'hrsh7th/cmp-nvim-lsp'},     -- Required
            {'hrsh7th/cmp-buffer'},       -- Optional
            {'hrsh7th/cmp-path'},         -- Optional
            {'saadparwaiz1/cmp_luasnip'}, -- Optional
            {'hrsh7th/cmp-nvim-lua'},     -- Optional
            {'andersevenrud/cmp-tmux'}, -- Added Essential
            {'hrsh7th/cmp-cmdline'},

            -- Snippets
            {'L3MON4D3/LuaSnip'},             -- Required
            {'rafamadriz/friendly-snippets'}, -- Optional
        }
    },
    -- terminal
    {
        'akinsho/toggleterm.nvim',
        tag = "*",
        config = true
    },
    -- treesitter
    {
        "nvim-treesitter/nvim-treesitter",
    },
    -- undotree
    {
        "jiaoshijie/undotree",
      dependencies  = {
            "nvim-lua/plenary.nvim",
        },
    },

    -- Which-key
    {
        'folke/which-key.nvim'
    },
}

