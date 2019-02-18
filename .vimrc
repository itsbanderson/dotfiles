" === BEGIN Vundle setup ===
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

" Syntax highlighting for scala
Plugin 'derekwyatt/vim-scala'

" Some must-have plugins
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/syntastic'
Plugin 'ervandew/supertab'
Plugin 'kien/ctrlp.vim'
"Plugin 'maxbrunsfeld/vim-yankstack'
Plugin 'vim-scripts/YankRing.vim'
Plugin 'fatih/vim-go'

" Plugins very specific to my setup
Plugin 'christoomey/vim-tmux-navigator'

" Experimenting with these
Plugin 'tpope/vim-surround'
Plugin 'bling/vim-airline'

" Annoyingly slow in large website codebase :(
" Plugin 'easymotion/vim-easymotion'
Plugin 'airblade/vim-gitgutter'

" Really nice plugin, but a little tricky to setup. On Mac OS X
"   1) Install homebrew python: `brew install python`
"   2) Install homebrew macvim: `brew install macvim
"   3) Symlink vim to macvim: `ln -s /usr/local/bin/vim /usr/local/vin/mvim'
"   4) Install CMake: `brew install cmake`
"   5) Run YouCompleteMe installer:
"       `~/.vim/bundle/YouCompleteMe/install.sh --clang-completer --gocode-completer`
" Requires vim to be built with python
" TEMP: Commenting out some plugins to see if it fixes this issue
"Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/nerdcommenter'

Plugin 'majutsushi/tagbar'
Plugin 'xolox/vim-misc'
" I think this slows things down a lot. Commenting out to check.
" Plugin 'xolox/vim-easytags'

" Rarely used, but keep around just in case
Plugin 'scrooloose/nerdtree'

" All Plugins must be added before this
call vundle#end()
filetype plugin indent on

" === END Vundle Setup ===
"
" Syntastic setup for PHP linting
let g:syntastic_php_checkers = ['php', 'phpcs']
let g:syntastic_php_phpcs_args = "--standard=~/Thumbtack/website/config/phpcs/ruleset.xml"
let g:syntastic_php_phpmd_post_args = "~/Thumbtack/website/config/phpmd/ruleset.xml"
let g:syntastic_aggregate_errors = 1

"
" === Go setup ===
" From golang instructions
" Some Linux distributions set filetype in /etc/vimrc.
 " Clear filetype flags before changing runtimepath to force Vim to reload them.
filetype off
filetype plugin indent off
set runtimepath+=$GOROOT/misc/vim
filetype plugin indent on

" === Basic VIM setup ===
syntax enable

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
"set relativenumber
set number
set undofile
set invlist

" Filetype settings
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

" Set up go linter
set rtp+=$GOPATH/src/github.com/golang/lint/misc/vim
"autocmd BufWritePost,FileWritePost *.go execute 'Lint' | cwindow
"

" Leader
let mapleader = ","

" Color scheme
let g:molokai_original = 1
colorscheme molokai "gruvbox

" Show the 100th column
set colorcolumn=100

set list
set listchars=tab:▸\ ,eol:¬

" === Search ===
" Ignore case when searching
set ignorecase

" Highlight all search terms
set hlsearch

" Search incrementally (as you type)
set incsearch

" Comma-space unhighlights the current search terms
nnoremap <leader><space> :noh<cr>

" Store vim files centrally
" set undodir=~/.vim/tmp/undo//
" set backupdir=~/.vim/tmp/backup// " backups
" set directory=~/.vim/tmp/swap//   " swap files
" set backup                        " enable backups

" === Movement ===
" have the h and l cursor keys wrap between lines (like <Space> and <BkSpc> do
" by default), and ~ covert case over line breaks; also have the cursor keys
" wrap in insert mode:
set whichwrap=h,l,~,[,]

" Line wraps
map  1G!Gfmt -w 72
set wrap

" === Helpful shortcuts ===
" Set up ctlp
let g:ctrlp_map = '<leader>t'
let g:ctrlp_cmd = 'CtrlP'

" Set up yankstack
nmap <c-p> <Plug>yankstack_substitute_older_paste
nmap <c-P> <Plug>yankstack_substitute_newer_paste

" See class structure of the file
nmap <leader>s :TagbarToggle<CR>

" Make it easier to switch between splits
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Clean trailing whitespace
map <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Clean up json
" From: https://pascalprecht.github.io/2014/07/10/pretty-print-json-in-vim/
map <leader>j :%!python -m json.tool<CR>

" Make ; do the same as :
nnoremap ; :

" === Thumbtack-specific ====
" Ignore build/release versions of files (to avoid accidentally editing instead of source)
set wildignore+=public/theme/scripts/build/**,public/theme/scripts/release/**,public/theme/styles/build/**,public/theme/styles/release/**,deploy/environment/**
