function Map(mode, lhs, rhs, opts)
    local options = { noremap = true, silent = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.keymap.set(mode, lhs, rhs, options)
end

--Remap space as leader key
Map("", "<Space>", "<Nop>")
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Normal --
-- Move up-down screen lines, not file lines
Map('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
Map('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })
Map('n', '<Up>', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
Map('n', '<Down>', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })
Map('n', '<C-a>', "0", { noremap = true, expr = true, silent = true })
Map('n', '<C-e>', "$", { noremap = true, expr = false, silent = true })

-- Yank until the end of line
Map('n', 'Y', 'y$', { noremap = true })

--  Clean Highlight with Esc
Map("n", "<esc>", ":let @/=''<CR>:<backspace>")
Map("n", "\\", ":let @/=''<CR>:<backspace>")
-- Helper
Map("n", "Q", "<nop>")
Map("n", "<leader>x", "<cmd>!chmod +x %<CR>")

-- Better window navigation
Map("n", "<C-h>", "<C-w>h") -- left window
Map("n", "<C-k>", "<C-w>k") -- up window
Map("n", "<C-j>", "<C-w>j") -- down window
Map("n", "<C-l>", "<C-w>l") -- right window

-- Resize with arrows when using multiple windows
Map("n", "<C-Up>", ":resize -2<CR>")
Map("n", "<c-down>", ":resize +2<cr>")
Map("n", "<c-right>", ":vertical resize -2<cr>")
Map("n", "<c-left>", ":vertical resize +2<cr>")

-- navigate buffers
Map("n", "<tab>", ":bnext<cr>") -- Next Tab 
Map("n", "<s-tab>", ":bprevious<cr>") -- Previous tab
Map("n", "<leader>h", ":nohlsearch<cr>") -- No highlight search
Map("n", "<c-w>", ":bdelete!<cr>") -- Delete buffer
Map("n", "<leader>bd", ":bd<cr>")

-- move text up and down
Map("n", "<a-j>", "<esc>:m .+1<cr>==gi") -- Alt-j 
Map("n", "<a-k>", "<esc>:m .-2<cr>==gi") -- Alt-k

-- insert --
-- press jk fast to exit insert mode 
Map("i", "jk", "<esc>") -- Insert mode -> jk -> Normal mode
Map("i", "kj", "<esc>") -- Insert mode -> kj -> Normal mode
Map("i", "<S-Tab>","<C-V><Tab>")

-- visual --
-- Old habbits
Map('v', '<C-a>', "0", { noremap = true, expr = true, silent = true })
Map('v', '<C-e>', "$", { noremap = true, expr = false, silent = true })

--- moving text in visual
Map("v", "J", ":m '>+1<CR>gv=gv")
Map("v", "K", ":m '<-2<CR>gv=gv")
-- stay in indent mode
Map("v", "<", "<gv") -- Right Indentation
Map("v", ">", ">gv") -- Left Indentation

-- move text up and down
Map("v", "<a-j>", ":m .+1<cr>==")
Map("v", "<a-k>", ":m .-2<cr>==")

-- Visual Block --
-- Move text up and down
--Terminal --
Map("x", "J", ":m '>+1<CR>gv-gv")
Map("x", "K", ":m '<-2<CR>gv-gv")
Map("x", "<A-j>", ":m '>+1<CR>gv-gv")
Map("x", "<A-k>", ":m '<-2<CR>gv-gv")

--Better terminal navigation
Map("t", "<C-h>", "<C-\\><C-N><C-w>h")
Map("t", "<C-j>", "<C-\\><C-N><C-w>j")
Map("t", "<C-k>", "<C-\\><C-N><C-w>k")
Map("t", "<C-l>", "<C-\\><C-N><C-w>l")
