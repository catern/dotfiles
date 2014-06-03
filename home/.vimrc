syntax on
set encoding=utf-8
set background=dark

let mapleader="\\"

set nocompatible       		  " vim, thank you very much

set autochdir
" line numbers
set number 
" we have a fast terminal
set ttyfast                 
" sets buffers to be hiddden not closed 
set hidden                   
" keep at least 5 lines left/right
"set scrolloff=5              
" keep at least 5 lines left/right
"set sidescrolloff=5          
" show current command
set showcmd                  
" if screen fills, use more prompt
set more                      
set showmode
set mouse=a

set visualbell
" disable visual bell; means bell will be totally disabled
" set t_vb=

set wildmenu

set display+=lastline

" Enable filetype detection
filetype on                  
" Enable filetype-specific indenting
filetype indent on           
" Enable filetype-specific plugins
filetype plugin on           

"  searching
" incremental search
set incsearch                
" search ignoring case
set ignorecase               
" if uppercase letter, don't ignore case
set smartcase                
" highlight the search
set hlsearch                 
" show matching bracket
set showmatch                
" ignore all whitespace and sync
set diffopt=filler,iwhite    

set backup
set backupdir=~/.vim/backup//
set directory=~/.vim/backup//
set viminfo+=n~/.vim/viminfo

if v:version >= 703
    set undofile
    set undodir=~/.vim/backup//
else
endif

" always show the status line
set laststatus=2             

" tabs
set tabstop=4
set expandtab
set smarttab
set shiftwidth=4
set softtabstop=4

set clipboard=unnamed

command W w !sudo tee % > /dev/null

nmap <F5> :w<CR>:make<CR>
imap <F5> <ESC>:w<CR>:make<CR>

nmap <F4> :bn<CR>
imap <F4> <ESC>:bn<CR>

nmap <F3> :bp<CR>
imap <F3> <ESC>:bp<CR>

map [27;2;13~ <S-CR>
map [27;5;13~ <C-CR>
map [27;6;13~ <C-S-CR>

if expand('%:t') =~?'bash-fc-\d\+'
      setfiletype sh
  endif

au! BufRead,BufNewFile *.tw   setfiletype twee 
autocmd BufRead,BufNewFile *.tw    compiler twee

autocmd BufRead,BufNewFile *.c0   runtime! syntax/c.vim
autocmd BufNewFile,BufRead *.c0 set makeprg=rlwrap\ ~/src/cc0/bin/coin\ -d\ %
autocmd BufNewFile,BufRead *.c0 setfiletype c

autocmd BufNewFile,BufRead *.sml set makeprg=rlwrap\ sml\ %
autocmd BufNewFile,BufRead *.hs set makeprg=ghc\ %
autocmd BufNewFile,BufRead *.tex set makeprg=pdflatex\ %
autocmd BufNewFile,BufRead *.dot set makeprg=dot\ -Tpng\ %\ >\ %.png
autocmd BufNewFile,BufRead *.scm set makeprg=rlwrap\ guile\ -l\ %
autocmd BufNewFile,BufRead *.c set makeprg=gcc\ -g\ %

silent !mkdir -p ~/.vim/backup
call system('touch ' . '~/.vim/local.vim')
so ~/.vim/local.vim
