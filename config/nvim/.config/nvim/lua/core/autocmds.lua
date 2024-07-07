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
