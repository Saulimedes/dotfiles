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
    -- base64
    {
      "taybart/b64.nvim"
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
    -- Comment
    {
    'numToStr/Comment.nvim',
    lazy = false,
    config = function()
       require("Comment").setup({
       })
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
            'nvim-lua/plenary.nvim',
            'debugloop/telescope-undo.nvim'}
    },
    -- Indentation Highlighting
    {
        "lukas-reineke/indent-blankline.nvim",
    },
    -- DAP
    {
      "mfussenegger/nvim-dap",
    },
    -- Git Integration
    {
        "lewis6991/gitsigns.nvim",
    },
    {
      "nvim-neo-tree/neo-tree.nvim",
      branch = "v3.x",
      dependencies = {
        "nvim-lua/plenary.nvim",
        "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
        "MunifTanjim/nui.nvim",
        },
        config = function()
           require("neo-tree").setup({
             close_if_last_window = true,
           })
        end
    },
    -- Language Support
    {
        'VonHeikemen/lsp-zero.nvim',
        lazy = true,
        branch = 'v2.x',

        dependencies = {
            -- LSP Support
            {'neovim/nvim-lspconfig'},
            {'williamboman/mason.nvim',
              build = function()
                pcall(vim.api.nvim_command, 'MasonUpdate')
              end,
            },         
            {'williamboman/mason-lspconfig.nvim'}, 

            -- Autocompletion
            {'hrsh7th/nvim-cmp'},
            {'hrsh7th/cmp-nvim-lsp'},
            {'hrsh7th/cmp-buffer'},
            {'hrsh7th/cmp-path'},
            {'saadparwaiz1/cmp_luasnip'},
            {'hrsh7th/cmp-nvim-lua'},
            {'andersevenrud/cmp-tmux'},
            {'hrsh7th/cmp-cmdline'},

            -- Snippets
            {'L3MON4D3/LuaSnip'},
            {'rafamadriz/friendly-snippets'},
        }
    },
    {  'nvim-lualine/lualine.nvim',
       dependencies = {"nvim-tree/nvim-web-devicons", "f-person/git-blame.nvim"},
    },
    -- Colorscheme
    {   'shaunsingh/nord.nvim',
        opts = {
          hot_reload = {
          enabled = true,
          },
        },
        config = function()
          vim.cmd([[colorscheme nord]])
          vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
          vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
        end
    },
    -- terminal
    {
        'akinsho/toggleterm.nvim',
        version = "*",
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

