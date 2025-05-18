require'hop'.setup { keys = 'etovxqpdygfblzhckisuran', term_seq_bias = 0 }

-- Changing the default f keyword
vim.api.nvim_set_keymap('n', 'f', "<cmd>lua require'hop'.hint_char1()<cr> :normal! <ESC>", {})

-- Pattern Matching with t keyword
vim.api.nvim_set_keymap('n', 't', "<cmd>HopPattern<CR> :normal! <ESC>", {noremap = true})
