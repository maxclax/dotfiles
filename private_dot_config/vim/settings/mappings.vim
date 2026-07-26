" -*- mode: vimrc -*-"

" Semicolon is too long to type.
nnoremap ; :
vnoremap ; :

" Map leader key.
let mapleader = "\<Space>"

" => Leader shortcuts ----------------------------------------------------- {{{1

nnoremap <Leader>] <C-]>
nnoremap <Leader>i <C-i>
nnoremap <Leader>o <C-o>
nnoremap <Leader>r :redraw!<cr>
nnoremap <Leader>w :w<cr>
nnoremap <Leader>dd "_dd
nnoremap <Leader>x q:
nnoremap <Leader>h q/
nnoremap <Leader>a :Ack! <C-r><C-w><cr>
nnoremap <leader>p :CtrlP<cr>
nnoremap <leader>b :CtrlPBuffer<cr>
nnoremap <leader>m :CtrlPMRU<cr>
nnoremap <leader>t :CtrlPTag<cr>
nnoremap <leader>n :NERDTreeToggle<cr>

nnoremap <leader>ev :vsplit $MYVIMRC<CR>        " Map <leader>ev to edit the .vimrc file
nnoremap <leader>sv :source $MYVIMRC<cr>        " Map <leader>sr to source the .vimrc

" => Movement and search -------------------------------------------------- {{{1

" Ignore case when searching.
set ignorecase
set smartcase

" Searching highlights
set hlsearch

" Fast split navigation.
nnoremap <c-j> <c-w><c-j>
nnoremap <c-k> <c-w><c-k>
nnoremap <c-l> <c-w><c-l>
nnoremap <c-h> <c-w><c-h>
tnoremap <c-j> <c-w><c-j>
tnoremap <c-k> <c-w><c-k>
tnoremap <c-l> <c-w><c-l>
tnoremap <c-h> <c-w><c-h>

" Absolute movement for word-wrapped lines.
nnoremap j gj
nnoremap k gk

" Accidentally hitting capital K doesn't open manpages.
nnoremap <s-k> k

" Unable to use stantard cursore only hjkl enjoy
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" Faster way return to NORMAL mode
inoremap jj <Esc>
inoremap jk <Esc>
inoremap <esc> <nop>

" make . to work with visually selected lines
vnoremap . :normal.<CR>

" Move visual selection
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Map Ctrl+Q to ":q"
nnoremap <C-q> :q<CR>

" Buffers
nnoremap <S-Left> :bprevious<CR>
nnoremap <S-Right> :bnext<CR>
nnoremap <S-Down> :buffers<CR>
nnoremap <S-Up> :buffer
nnoremap <S-Delete> :bdelete<CR>

" Tabs
" nnoremap <C-t> :tabnew<CR>
nnoremap <C-w> :tabclose<CR>
" nnoremap <C-p> :tabprevious<CR>
" nnoremap <C-n> :tabnew<CR>

" highlight trailing whitespace
match ErrorMsg '\s\+$'
" remove trailing whitespaces automatically
autocmd BufWritePre * :%s/\s\+$//e
