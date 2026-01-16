local status_ok, wk = pcall(require, "which-key")
if not status_ok then
    return
end


-- Setup which-key
wk.setup({
  plugins = {
    marks = true,
    registers = true,
    spelling = {
      enabled = true,
      suggestions = 20,
    },
    presets = {
      operators = true,
      motions = true,
      text_objects = true,
      windows = true,
      nav = true,
      z = true,
      g = true,
    },
  },
  icons = {
    breadcrumb = "»",
    separator = "➜",
    group = "+",
  },
  win = {
    padding = { 0, 0, 0, 0 },
    no_overlap = true,
    border = "none",
    zindex = 1000,
  },
  layout = {
    width = { min = 20 },
    spacing = 3,
  },
  show_help = false,
  show_keys = false,
  triggers = {
    { "<leader>", mode = { "n", "v" } },
    { "g", mode = "n" },
    { "z", mode = "n" },
    { "[", mode = "n" },
    { "]", mode = "n" },
  },
})

-- Define mappings
wk.add({
  { "<leader>a", "<cmd>Alpha<cr>", desc = "Alpha" },
  { "<leader>e", "<cmd>Neotree<cr>", desc = "Explorer" },
  { "<leader>k", "<cmd>bdelete<CR>", desc = "Kill Buffer" },
  { "<leader>m", "<cmd>Mason<CR>", desc = "Mason" },
  { "<leader>p", "<cmd>Lazy<CR>", desc = "Plugin Manager" },
  { "<leader>q", "<cmd>wqall!<CR>", desc = "Quit" },
  { "<leader>u", "<cmd>Telescope undo<CR>", desc = "Undo Tree" },
  { "<leader>w", "<cmd>w!<CR>", desc = "Save" },

  { "<leader>r", group = "Reformat Code" },
  { "<leader>ra", "<cmd>lua vim.lsp.buf.format{async=true}<cr>", desc = "Reformat Code" },
  { "<leader>rb", '<cmd>lua require("b64").decode()<cr>', desc = "Base64 Decode" },
  { "<leader>rB", '<cmd>lua require("b64").encode()<cr>', desc = "Base64 Encode" },

  { "<leader>g", group = "Git" },
  { "<leader>gj", "<cmd>lua require 'gitsigns'.next_hunk()<cr>", desc = "Next Hunk" },
  { "<leader>gk", "<cmd>lua require 'gitsigns'.prev_hunk()<cr>", desc = "Prev Hunk" },
  { "<leader>gl", "<cmd>lua require 'gitsigns'.blame_line()<cr>", desc = "Blame" },
  { "<leader>gp", "<cmd>lua require 'gitsigns'.preview_hunk()<cr>", desc = "Preview Hunk" },
  { "<leader>gr", "<cmd>lua require 'gitsigns'.reset_hunk()<cr>", desc = "Reset Hunk" },
  { "<leader>gR", "<cmd>lua require 'gitsigns'.reset_buffer()<cr>", desc = "Reset Buffer" },
  { "<leader>gs", "<cmd>lua require 'gitsigns'.stage_hunk()<cr>", desc = "Stage Hunk" },
  { "<leader>gu", "<cmd>lua require 'gitsigns'.undo_stage_hunk()<cr>", desc = "Undo Stage Hunk" },
  { "<leader>go", "<cmd>Telescope git_status<cr>", desc = "Open changed file" },
  { "<leader>gb", "<cmd>Telescope git_branches<cr>", desc = "Checkout branch" },
  { "<leader>gc", "<cmd>Telescope git_commits<cr>", desc = "Checkout commit" },
  { "<leader>gd", "<cmd>Gitsigns diffthis HEAD<cr>", desc = "Diff" },

  { "<leader>l", group = "LSP" },
  { "<leader>la", "<cmd>lua vim.lsp.buf.code_action()<cr>", desc = "Code Action" },
  { "<leader>li", "<cmd>LspInfo<cr>", desc = "Info" },
  { "<leader>ll", "<cmd>lua vim.lsp.codelens.run()<cr>", desc = "CodeLens Action" },
  { "<leader>lr", "<cmd>lua vim.lsp.buf.rename()<cr>", desc = "Rename" },
  { "<leader>ls", "<cmd>Telescope lsp_document_symbols<cr>", desc = "Document Symbols" },
  { "<leader>lS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", desc = "Workspace Symbols" },
  { "<leader>ld", "<cmd>Trouble document_diagnostics<cr>", desc = "Document Diagnostics" },
  { "<leader>lw", "<cmd>Trouble workspace_diagnostics<cr>", desc = "Workspace Diagnostics" },
  { "<leader>lf", "<cmd>lua vim.lsp.buf.format{async=true}<cr>", desc = "Format" },
  { "<leader>lt", "<cmd>Telescope lsp_type_definitions<cr>", desc = "Type Definition" },
  { "<leader>lj", "<cmd>lua vim.diagnostic.goto_next()<cr>", desc = "Next Diagnostic" },
  { "<leader>lk", "<cmd>lua vim.diagnostic.goto_prev()<cr>", desc = "Prev Diagnostic" },

  { "<leader>f", group = "File Search" },
  { "<leader>fc", "<cmd>Telescope colorscheme<cr>", desc = "Colorscheme" },
  { "<leader>ff", "<cmd>lua require('telescope.builtin').find_files()<cr>", desc = "Find files" },
  { "<leader>fg", "<cmd>lua require('telescope.builtin').git_files()<cr>", desc = "Find Git files" },
  { "<leader>fb", "<cmd>lua require('telescope.builtin').buffers()<cr>", desc = "Find Buffer" },
  { "<leader>ft", "<cmd>Telescope live_grep <cr>", desc = "Find Text Pattern" },
  { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent Files" },
  { "<leader>fp", "<cmd>Telescope projects<cr>", desc = "Projects" },
  { "<leader>fT", "<cmd>TodoTelescope<cr>", desc = "Todo Comments" },

  { "<leader>s", group = "Search" },
  { "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Find Help" },
  { "<leader>sm", "<cmd>Telescope man_pages<cr>", desc = "Man Pages" },
  { "<leader>sr", "<cmd>Telescope registers<cr>", desc = "Registers" },
  { "<leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps" },
  { "<leader>sc", "<cmd>Telescope commands<cr>", desc = "Commands" },

  { "<leader>t", group = "Terminal" },
  { "<leader>tn", "<cmd>lua _NODE_TOGGLE()<cr>", desc = "Node" },
  { "<leader>tp", "<cmd>lua _PYTHON_TOGGLE()<cr>", desc = "Python" },
  { "<leader>tf", "<cmd>ToggleTerm direction=float<cr>", desc = "Float" },
  { "<leader>th", "<cmd>ToggleTerm size=10 direction=horizontal<cr>", desc = "Horizontal" },
  { "<leader>tv", "<cmd>ToggleTerm size=80 direction=vertical<cr>", desc = "Vertical" },

  { "<leader>bd", ":bd<CR>", desc = "Delete Buffer" },
  { "<leader>bn", ":bnext<CR>", desc = "Next Buffer" },
  { "<leader>bp", ":bprevious<CR>", desc = "Previous Buffer" },


  -- Visual mode mappings
  {
    mode = "v",
    { "<leader>a", "<cmd>lua vim.lsp.buf.format{async=true}<cr>", desc = "Reformat Code" },
    { "<leader>c", ":CommentToggle<CR>", desc = "Toggle Comment" },
    { "<leader>b", group = "Base64" },
    { "<leader>bd", '<cmd>lua require("b64").decode()<cr>', desc = "Base64 Decode" },
    { "<leader>be", '<cmd>lua require("b64").encode()<cr>', desc = "Base64 Encode" },
  },
})
