-- core
require "core.bootstrap"
require "core.keymaps"
require "core.options"
require "core.autocmds"

-- plugins
require("lazy").setup('plugins.lazy')
require "plugins"

-- neovide
if vim.g.neovide then
  vim.g.neovide_floating_blur_amount_x = 2.0
  vim.g.neovide_floating_blur_amount_y = 2.0
  vim.g.neovide_confirm_quit = true
  vim.g.neovide_hide_mouse_when_typing = true
  vim.g.neovide_cursor_animation_length = 0.13
  vim.g.neovide_cursor_trail_size = 0.8
  vim.g.neovide_cursor_animate_command_line = true
  vim.g.neovide_cursor_antialiasing = true
  vim.g.neovide_cursor_vfx_mode = "torpedo"
end

