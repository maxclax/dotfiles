" -*- mode: vimrc -*-"

" => Pre-load ------------------------------------------------------------- {{{1

" Enable syntax highlighting.
syntax on

" Enable file type based options.
filetype plugin indent on

" Download and install vim-plug (cross platform).
if empty(glob(
    \ '$HOME/' . (has('win32') ? 'vimfiles' : '.vim') . '/autoload/plug.vim'))
  execute '!curl -fLo ' .
    \ (has('win32') ? '\%USERPROFILE\%/vimfiles' : '$HOME/.vim') .
    \ '/autoload/plug.vim --create-dirs ' .
    \ 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" => Sane defaults (from Neovim) ------------------------------------------ {{{1

set autoindent
set autoread
set backspace=indent,eol,start
set belloff=all
set complete-=i
set display=lastline
set formatoptions=tcqj
set history=10000
set incsearch
set laststatus=2
set nocompatible
set ruler
set sessionoptions-=options
set showcmd
set sidescroll=1
set smarttab
set ttimeoutlen=50
set ttyfast
set viminfo+=!
set wildmenu

" => Editing -------------------------------------------------------------- {{{1

" Global indentation settings.
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2

autocmd filetype python set expandtab              " Expand tabs to spaces. Essential in Python.
autocmd filetype python set tabstop=4              " Number of spaces tab is counted for.
autocmd filetype python set shiftwidth=4           " Number of spaces to use for autoindent.

" Disable backups and .swp files.
set nobackup
set noswapfile
set nowritebackup

" Use system clipboard.
set clipboard=unnamed,unnamedplus

" Enable wild menu (tab command autocompletion).
set wildmode=list:longest,full

" Don't complain about unsaved files when switching buffers.
set hidden

" Enable persistent undo.
" set undofile
" set undodir=$HOME/.vim/undodir

" Automatically change the working directory for the current file.
" set autochdir

" => Looks ---------------------------------------------------------------- {{{1

set background=dark
colorscheme PaperColor
set guifont=Berkeley\ Mono:h14

" Shorten press ENTER to continue messages.
set shortmess=atI

" Highlight cursor line.
set cursorline
set cursorcolumn

" Display line numbers if terminal is wide enough.
if &co > 80
  set number
endif

" Soft word wrap.
set linebreak

" Make soft line breaks much better looking.
set breakindent

" Pretty soft break character.
let &showbreak = '> '

" Use Unix as the standart file type.
set ffs=unix,dos,mac

" Fold using {{{n, where n is fold level
set foldmethod=marker
" set foldlevel=3
autocmd filetype python set foldmethod=indent

" => Fixes and hacks ------------------------------------------------------ {{{1

" Increase lower status bar height in diff mode.
if &diff
  set cmdheight=2
endif

" Fix easymotion target colors in a PaperColor theme.
if g:colors_name ==# 'PaperColor'
  hi EasyMotionTarget2First ctermbg=none ctermfg=red
  hi EasyMotionTarget2Second ctermbg=none ctermfg=red
endif

" Disable line numbers in terminal.
au CursorMoved * if &buftype == 'terminal' | set nonumber | endif

" Open all folds when entering Vim.
set nofoldenable

" Don't wrap lines  (to fix the way shortened URLs are displayed).
autocmd FileType vimwiki set nowrap
autocmd FileType python setlocal nowrap
autocmd FileType html setlocal nowrap
