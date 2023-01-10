" General
set rtp^=~/.vim,~/.vim/after
set completeopt=menu,menuone,preview,noinsert,noselect
set cpoptions-=m
set grepprg=rg\ --vimgrep\ --smart-case\ --follow
set grepformat=%f:%l:%c:%m,%f:%l:%m
set nopaste
set signcolumn=yes
set hidden
set autoread
set autowrite
set ff=unix
set selection=exclusive
set selectmode=mouse,key
set mouse+=a
set encoding=utf-8
set emoji
set lazyredraw
set ttyfast
set formatoptions+=j
set wildoptions=tagfile
set formatoptions-=t
set history=1000
set nrformats=alpha,octal,hex
set wrap
set whichwrap+=b,s,<,>,[,],h,~
set linebreak
set breakindent
set breakindentopt=shift:2
set showbreak=↳
set ignorecase
set infercase
set textwidth=0
set noerrorbells
set wrapmargin=0
set hlsearch
set incsearch
set relativenumber
set titlestring=%{expand(\"%:t\")}
set printoptions=left:5pc,right:5pc,top:5pc,bottom:5pc
set noruler
set scrolloff=3
set shortmess=aoOtIWcFs
set showcmd
set showmatch
set noshowmode
set shell=/bin/fish
set updatetime=300
set title
set virtualedit=all
set wildmode=full
set noswapfile
set path+=++
set splitright
set splitbelow
set exrc
set laststatus=2
set clipboard+=unnamedplus
let mapleader=","
let g:mapleader = ','

" Plugins
set nocompatible
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
"" text manipulation
Plug 'godlygeek/tabular'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sleuth'
Plug 'sheerun/vim-polyglot'

"" helper
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'nvim-lualine/lualine.nvim'

Plug 'mhinz/vim-startify'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'plasticboy/vim-markdown'
Plug 'honza/vim-snippets'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'dylanaraps/wal.vim'
Plug 'w0rp/ale'

"" tmux
Plug 'christoomey/vim-tmux-navigator'
Plug 'roxma/vim-tmux-clipboard'
Plug 'wellle/tmux-complete.vim'
call plug#end()
filetype plugin indent on

"" colors
filetype plugin indent on
syntax enable
set encoding=utf-8
colorscheme wal

""" Manual color adjustments
set cursorline
hi Cursorline ctermbg=3 cterm=NONE ctermfg=black
hi Search cterm=bold ctermfg=black ctermbg=4
hi CursorLineNr ctermfg=7 ctermbg=0 cterm=NONE
hi Pmenu ctermfg=NONE ctermbg=NONE
hi PmenuThumb ctermbg=0
hi PmenuSBar ctermbg=NONE ctermfg=NONE
hi PmenuSel ctermfg=NONE ctermbg=3
hi TabLineFill ctermfg=NONE ctermbg=NONE cterm=NONE
hi TabLine ctermfg=NONE ctermbg=NONE cterm=NONE
hi TabLineSel ctermfg=7 ctermbg=3 cterm=bold

"" Scrolling
set sidescroll=1    " Smooth side scrolling
set sidescrolloff=5 " Left-right offset during side-scrolling

"" Characters for whitespace
set listchars+=tab:↦\
set listchars+=eol:↵
set listchars+=extends:»    " Line extends to right
set listchars+=precedes:«   " Line extends to left

"" Characters for filling
set fillchars+=vert:╽  " Vertical split
set fillchars+=diff:⣿  " Empty line in vimdiff

"" Tab (2 spaces)
set shiftwidth=2
set tabstop=2
set softtabstop=2
set expandtab
set wildcharm=<Tab>

"" Autoread
augroup autoRead
    autocmd!
    autocmd CursorHold * silent! checktime
augroup END

"" GUI Options
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

" folding
if has ('folding')
  set foldenable
  set foldmethod=marker
  set foldmarker={{{,}}}
  set foldcolumn=0
endif
set foldnestmax=2              " only fold up to method headers
set foldlevel=2
set foldmethod=manual
set nofoldenable                " don't fold on file open
noremap <leader>z za
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <Space> zf
set fillchars=vert:┃
set fillchars+=fold:·

"" Clipboard
nnoremap Y y$

"" Backspace over anything in insert mode
set backspace=eol,start

"" Beginning and end of line
nnoremap <C-a> <ESC>^
nnoremap <C-e> <ESC>$
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
inoremap <C-a> <Home>
inoremap <C-e> <End>

"" On search, highlight but do not jump ahead
nnoremap * *``

"" Move up-down screen lines, not file lines
nnoremap <expr> j v:count ? (v:count > 5 ? "m'" . v:count : '') . 'j' : 'gj'
nnoremap <expr> k v:count ? (v:count > 5 ? "m'" . v:count : '') . 'k' : 'gk'

"" make . to work with visually selected lines
vnoremap . :normal.<CR>
"" Vmap for maintain Visual Mode after shifting > and <
vmap < <gv
vmap > >gv

"" Move visual selection
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Text alignment
nnoremap <Leader>Al :left<CR>
nnoremap <Leader>Ac :center<CR>
nnoremap <Leader>Ar :right<CR>
vnoremap <Leader>Al :left<CR>
vnoremap <Leader>Ac :center<CR>
vnoremap <Leader>Ar :right<CR>

"" Insert Mode Completion
inoremap <C-]>     <C-x><C-]>
inoremap <C-Space> <C-x><C-o>
inoremap <C-d>     <C-x><C-k>
inoremap <C-f>     <C-x><C-f>
inoremap <C-l>     <C-x><C-l>

"" Insert/Command Mode Navigation
imap <C-h> <left>
imap <C-j> <down>
imap <C-k> <up>
imap <C-l> <right>
cmap <C-h> <left>
cmap <C-j> <down>
cmap <C-k> <up>
cmap <C-l> <right>

""" Allow movement seamlessly with terminals
if exists(':tnoremap')
  tnoremap <C-h> <C-\><C-n><C-w>h
  tnoremap <C-j> <C-\><C-n><C-w>j
  tnoremap <C-k> <C-\><C-n><C-w>k
  tnoremap <C-l> <C-\><C-n><C-w>l
endif

"" buffers
nnoremap <silent> ; :Buffers<CR>
nnoremap <leader>Bc :bd<CR>
nnoremap <Leader>Bn :bnext<CR>
nnoremap <Leader>Bp :bprevious<CR>
nnoremap <Leader>q :xa<CR>
nnoremap <leader>Bc :BufOnly<CR>
nnoremap <leader>Bl :BLines<CR>

"" Split Navigations
nnoremap <C-J> <C-W><C-J> # Move to split below
nnoremap <C-K> <C-W><C-K> # Move to split above
nnoremap <C-L> <C-W><C-L> # Move to split right
nnoremap <C-H> <C-W><C-H> # Move to split left
nnoremap <leader>m :vsp<cr>
nnoremap <leader>i :sp<cr>
nnoremap <leader>o :on<cr>
noremap Zz <c-w>_ \| <c-w>\|
noremap Zo <c-w>=

"" Tabs
nnoremap <leader>t :tabnew<cr>
nnoremap <leader>\ :tabonly<cr>
nnoremap <F1> :buffers<CR>:buffer<Space>
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt
nnoremap <leader>w gt
nnoremap <leader>W gT

"" Helper
""" Execute as sudo idiot
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!
""" Clean Highlight with Esc
nnoremap <esc> :let @/=''<CR>:<backspace>
nnoremap \ :let @/=''<CR>:<backspace>

""" redo/undo
imap <c-u> <esc>:undo<cr>i
nmap <c-u> :undo<cr>
nmap <c-r> :redo<cr>

" ctrl+backspace
nmap <C-h> <C-w>
cmap <C-h> <C-w>
imap <C-h> <C-w>

""" Use Enter to Insert new Line
nnoremap <cr> o<esc>
autocmd CmdwinEnter * nnoremap <CR> <CR>
autocmd CmdwinLeave * nnoremap <cr> o<esc>
au FocusGained,BufEnter * :checktime

""" Repeat Commands with .
nnoremap <silent> s* :let @/='\<'.expand('<cword>').'\>'<CR>cgn
xnoremap <silent> s* "sy:let @/=@s<CR>cgn
xnoremap . :norm.<CR>

" abbreviations
ab oe ö
ab ae ä
ab ue ü
iab ste set
iab fro for
iab esle else
iab pbec Paul Becker
iab fucntion function
iab funciton function
iab publci public

" plugin settings
"" coc
let g:coc_global_extensions = [
  \ 'coc-snippets',
  \ 'coc-pairs',
  \ 'coc-tabnine',
  \ 'coc-git',
  \ 'coc-tsserver',
  \ 'coc-snippets',
  \ 'coc-eslint',
  \ 'coc-prettier',
  \ 'coc-json',
  \ ]
imap <C-f> <Plug>(coc-snippets-expand)
let g:coc_snippet_next = '<c-n>'
let g:coc_snippet_next = '<c-n>'
let g:coc_snippet_prev = '<c-p>'
imap <C-space> <Plug>(coc-snippets-expand-jump)

"" ale
let g:ale_sign_highlight_linenrs = 1
let g:ale_lint_on_enter = 0
let g:ale_linters = {}
let g:ale_sign_error = '✘'
let g:ale_sign_warning = ""
let g:ale_fix_on_save = 1
let g:ale_set_highlights = 0
let g:ale_lint_on_text_changed = 'always'
let g:ale_sign_column_always = 1
let g:ale_echo_cursor = 1
nmap <F5> <Plug>(ale_fix)
nmap <silent> <C-M> <Plug>(ale_previous_wrap)
nmap <silent> <C-m> <Plug>(ale_next_wrap)

"" tmux
let g:tmuxcomplete#asyncomplete_source_options = {
            \ 'name':      'tmuxcomplete',
            \ 'whitelist': ['*'],
            \ 'config': {
            \     'splitmode':      'words',
            \     'filter_prefix':   1,
            \     'show_incomplete': 1,
            \     'sort_candidates': 0,
            \     'scrollback':      0,
            \     'truncate':        0
            \     }
            \ }
let g:tmuxcomplete#trigger = ''

"" vim fugitive
noremap <leader>gb :Gblame<CR>
noremap <leader>gs :Gstatus<CR>
noremap <leader>gc :Gcommit<CR>
noremap <leader>gl :Gpull<CR>
noremap <leader>gh :Gpush<CR>

"" vim airline
lua << END
require('lualine').setup()
END

"" fzf
nnoremap <C-p> :Files<Cr>
nnoremap <Leader>f :Rg<Cr>

"" nerdtree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
let g:NERDTreeDirArrowExpandable = ''
let g:NERDTreeDirArrowCollapsible = ''

nnoremap <leader>n :call NerdTreeToggle()<CR>

"" Remove trailing Spaces
autocmd BufWritePre * %s/\s\+$//e
autocmd BufWritePre *.* %s/\s\+$//e

" Show cursorline only in active Window
augroup CursorLineOnlyInActive
  autocmd!
    autocmd VimEnter,WinEnter,FocusGained,BufWinEnter * setlocal cursorline
  autocmd WinLeave,FocusLost *                      setlocal nocursorline
augroup END
