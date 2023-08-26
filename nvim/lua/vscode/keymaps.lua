-- Set up the VSCodeNotify function for convenience
local function vscode_notify(action)
    vim.cmd("call VSCodeNotify('" .. action .. "')")
end

pcall(function() vim.api.nvim_del_keymap('n', '<C-l>') end)
pcall(function() vim.api.nvim_del_keymap('x', '<C-l>') end)

-- Navigation
vim.api.nvim_set_keymap('n', '<C-j>', ':call VSCodeNotify("workbench.action.navigateDown")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('x', '<C-j>', ':call VSCodeNotify("workbench.action.navigateDown")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-k>', ':call VSCodeNotify("workbench.action.navigateUp")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('x', '<C-k>', ':call VSCodeNotify("workbench.action.navigateUp")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-h>', ':call VSCodeNotify("workbench.action.navigateLeft")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('x', '<C-h>', ':call VSCodeNotify("workbench.action.navigateLeft")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-l>', ':call VSCodeNotify("workbench.action.navigateRight")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('x', '<C-l>', ':call VSCodeNotify("workbench.action.navigateRight")<CR>', { noremap = true, silent = true })

-- Toggle Editor Widths
vim.api.nvim_set_keymap('n', '<C-w>_', ':call VSCodeNotify("workbench.action.toggleEditorWidths")<CR>', { noremap = true, silent = true })

-- Whichkey Show
vim.api.nvim_set_keymap('n', '<Space>', ':call VSCodeNotify("whichkey.show")<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('x', '<Space>', ':call VSCodeNotify("whichkey.show")<CR>', { noremap = true, silent = true })
