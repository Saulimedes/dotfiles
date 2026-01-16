local cmd = vim.cmd

-- Show Cursorline only in active Window
cmd [[
  augroup CursorLineOnlyInActive
    autocmd!
      autocmd VimEnter,WinEnter,FocusGained,BufWinEnter * setlocal cursorline
      autocmd WinLeave,FocusLost *                      setlocal nocursorline
  augroup end
]]

-- keep position in text
cmd [[
  augroup vimStartup
    au!

    autocmd BufReadPost *
      \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
      \ |   exe "normal! g`\""
      \ | endif

  augroup END
]]

-- Highlight when yanking text
cmd [[
  augroup highlight_yank
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank({higroup="IncSearch", timeout=150})
  augroup END
]]

-- Auto resize panes when resizing nvim window
cmd [[
  augroup resize_splits
    autocmd!
    autocmd VimResized * wincmd =
  augroup END
]]

-- Trim trailing whitespace on save
cmd [[
  augroup trim_whitespace
    autocmd!
    autocmd BufWritePre * :%s/\s\+$//e
  augroup END
]]
