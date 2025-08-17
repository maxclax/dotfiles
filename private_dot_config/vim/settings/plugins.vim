" -*- mode: vimrc -*-"

" All the plugins are managed via vim-plug, run :PlugInstall to install all
" the plugins from Github, :PlugUpdate to update. Leader key is the spacebar.

" => vim-plug plugins ----------------------------------------------------- {{{1

let g:plug_timeout = 300 " Increase vim-plug timeout

call plug#begin()

  Plug 'junegunn/vim-plug'                         " Need it for helpers download
  Plug 'tpope/vim-fugitive'                        " Git integration
  Plug 'tpope/vim-vinegar'                         " - to open netrw
  Plug 'vim-airline/vim-airline'                   " powerline
  Plug 'Lokaltog/vim-easymotion'                   " better move commands
  Plug 'christoomey/vim-tmux-navigator'            " better tmux integration
  Plug 'ervandew/supertab'                         " more powerful <tab>
  Plug 'junegunn/goyo.vim', {'on': 'Goyo'}         " distraction-free writing
  Plug 'justinmk/vim-dirvish'                      " less buggy netrw alternative
  Plug 'preservim/nerdtree', {'on': 'NERDTreeToggle'}
  Plug 'leafgarland/typescript-vim'                " typescript syntax
  Plug 'mileszs/ack.vim', {'on': 'Ack'}            " ack integration
  Plug 'simnalamburt/vim-mundo'                    " visualize the undo tree
  Plug 'tomtom/tcomment_vim'                       " commenting helpers
  Plug 'tpope/vim-abolish'                         " change case on the fly
  Plug 'tpope/vim-repeat'                          " repeat everything
  Plug 'tpope/vim-surround'                        " better surround commands
  Plug 'tpope/vim-unimpaired'                      " pairs of helpful commands
  Plug 'vimwiki/vimwiki'                           " personal wiki
  Plug 'dense-analysis/ale', {'for': ['python']}   " async syntax checke
  Plug 'ctrlpvim/ctrlp.vim'                        " Ctrl+p to fuzzy search
  Plug 'junegunn/fzf', {'do': {->fzf#install()}}
  Plug 'junegunn/fzf.vim'

  Plug 'vim-scripts/ScrollColors'                  " colorscheme scroller
  Plug 'Zabanaa/neuromancer.vim'                   " colorscheme
  Plug 'NLKNguyen/papercolor-theme'                " colorscheme
  Plug 'vim-scripts/Spacegray.vim'                 " colorscheme
  Plug 'altercation/vim-colors-solarized'          " colorscheme
  Plug 'squarefrog/tomorrow-night.vim'             " colorscheme

call plug#end()

" => Plugins configuration ------------------------------------------------ {{{1

" Load all plugins.
packloadall
" Load help files for all plugins.
silent! helptags ALL

" nerdtree
let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore files in NERDTree
let NERDTreeShowBookmarks=1 " Display bookmarks on startup.
let NERDTreeHijackNetrw=0
let g:NERDTreeFileLines = 1

" CtrlP
let g:ctrlp_working_path_mode = 'ra' " Set CtrlP working directory to a repository root

" fzf
let $FZF_DEFAULT_COMMAND = 'list_all_files'
nnoremap <c-p> :FZF<cr>
set grepprg=rg\ --vimgrep\ --smart-case\ --follow

" vim-test
let test#strategy = "dispatch"

" ack -> ag
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endi

" :Far -> ag
let g:far#source = 'ag'

" Mundo
let g:gundo_preview_bottom = 1
nnoremap <F5> :MundoToggle<cr>
