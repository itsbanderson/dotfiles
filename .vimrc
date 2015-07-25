" === Vundle setup ===
set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Plugin 'gmarik/Vundle.vim'

" Some nicer color schemes
Plugin 'morhetz/gruvbox'
Plugin 'nanotech/jellybeans.vim'
Plugin 'tomasr/molokai'
Plugin 'altercation/vim-colors-solarized'

" Some must-have plugins
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/syntastic'
Plugin 'ervandew/supertab'
Plugin 'kien/ctrlp.vim'

" Experimenting with these
Plugin 'tpope/vim-surround'
Plugin 'bling/vim-airline'
Plugin 'easymotion/vim-easymotion'
Plugin 'airblade/vim-gitgutter'
Plugin 'Valloric/YouCompleteMe'

" Rarely used, but keep around just in case
Plugin 'scrooloose/nerdtree'

" All Plugins must be added before this
call vundle#end()
filetype plugin indent on

" Set up ctlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'


" === Basic VIM setup ===
syntax enable

" Make it easier to switch between splits
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Tabs/spaces
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" Basics
set encoding=utf-8
set scrolloff=3
set autoindent
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set visualbell
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start
set laststatus=2
set relativenumber
set undofile
set invlist

autocmd FileType html setlocal expandtab shiftwidth=4 softtabstop=4
autocmd FileType htmldjango setlocal expandtab shiftwidth=4 softtabstop=4
autocmd FileType python setlocal expandtab shiftwidth=4 softtabstop=4
autocmd FileType javascript setlocal expandtab shiftwidth=4 softtabstop=4
autocmd FileType php setlocal expandtab shiftwidth=4 softtabstop=4
autocmd FileType yaml setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType ruby setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType coffee setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType css setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType go setlocal noexpandtab shiftwidth=4 tabstop=4 softtabstop=4
autocmd FileType haskell setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4

" Load markdown files with correct syntax highlighting
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Leader
let mapleader = ","

" Color scheme
set background=dark
colorscheme jellybeans

" Set up a way to toggle between relative and absolute line numbering
function! NumberToggle()
    if(&relativenumber == 1)
        set number
    else
        set relativenumber
    endif
endfunc

nnoremap <leader>n :call NumberToggle()<cr>

set list
set listchars=tab:▸\ ,eol:¬

" Ignore case when searching
set ignorecase

" Highlight all search terms
set hlsearch

" Search incrementally (as you type)
set incsearch

" Comma-space unhighlights the current search terms
nnoremap <leader><space> :noh<cr>

" have the h and l cursor keys wrap between lines (like <Space> and <BkSpc> do
" by default), and ~ covert case over line breaks; also have the cursor keys
" wrap in insert mode:
set whichwrap=h,l,~,[,]

" Line wraps
map  1G!Gfmt -w 72
set wrap

" comment/uncomment lines
map C 0i//j
map T 0xxj

" Clean trailing whitespace
map <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Use Ack
nnoremap <leader>a :Ack

" Make ; do the same as :
nnoremap ; :

" Always have syntax highlighting on with the gui
syntax on

" Open a scratch buffer
nmap <leader><tab> :Sscratch<cr><C-W>x<C-j>:resize 15<cr>

" Use one of the best monospace font available
set guifont=Anonymous\ Pro\ 16

" Show the 100th column
set colorcolumn=100

" Store vim files centrally
set undodir=~/.vim/tmp/undo//
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files
set backup                        " enable backups

" Limit height for command-t
let g:CommandTMaxHeight=20

" Autoindent new lines after keywords in python
autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
