" Persistent Mode
if has("persistent_undo")
  if !isdirectory($HOME . '/.vim/undo')
    call mkdir($HOME.'/.vim/undo', 'p')
  endif
  set undodir=~/.vim/undo//
  set undofile
  set undolevels=1000
  set undoreload=10000
endif

" Backup
if !isdirectory($HOME . '/.vim/backup')
  call mkdir($HOME.'/.vim/backup', 'p')
endif
set backupdir=~/.vim/backup//
set sessionoptions=blank,buffers,curdir,folds,help,resize,tabpages,winsize,globals
set ssop-=options
set ssop-=folds
set backupskip+=/tmp/*,/private/tmp/*,.vault.vim,$TMPDIR/*,$TMP/*,$TEMP/*,*/shm/*,/private/var/*
let g:session_autosave = "yes"
let g:session_autoload = "yes"
set backupcopy=auto

" Swap
if !isdirectory($HOME . '/.vim/swap')
  call mkdir($HOME.'/.vim/swap', 'p')
endif
set directory=~/.vim/swap//
set swapfile

if has('nvim')
  set shada+=n~/.vim/shada
else
  set viminfo+=n~/.vim/viminfo
endif

"" Restore last cursor position and marks on open
au BufReadPost *
         \ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit'
         \ |   exe "normal! g`\""
         \ | endif

"" Disable swap/undo/viminfo/shada files in temp directories or shm
augroup MyAutoCmd
	autocmd!
	silent! autocmd BufNewFile,BufReadPre
		\ /tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*/shm/*,/private/var/*,.vault.vim
		\ setlocal noswapfile noundofile nobackup nowritebackup viminfo= shada=
augroup END

"" If sudo, disable vim swap/backup/undo/shada/viminfo writing
if $SUDO_USER !=# '' && $USER !=# $SUDO_USER
		\ && $HOME !=# expand('~'.$USER)
		\ && $HOME ==# expand('~'.$SUDO_USER)

	set noswapfile
	set nobackup
	set nowritebackup
	set noundofile
	if has('nvim')
		set shada="NONE"
	else
		set viminfo="NONE"
	endif
endif

augroup remember_folds
  autocmd!
  autocmd BufWinLeave * mkview
  autocmd BufWinEnter * silent! loadview
augroup END
