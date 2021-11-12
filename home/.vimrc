set nocompatible

"{{{ Plugin List

call plug#begin('~/.vim/bundle')

" Git and GitHub
Plug 'tpope/vim-git'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
if has('nvim') || has('patch-8.0.902')
  Plug 'mhinz/vim-signify'
else
  Plug 'mhinz/vim-signify', { 'branch': 'legacy' }
endif

" UI and Colorscheme Plugins
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ayu-theme/ayu-vim'
Plug 'ayu-theme/ayu-vim-airline'
Plug 'audibleblink/hackthebox.vim'

" Utility Plugins
Plug 'mileszs/ack.vim' "Search
Plug 'junegunn/fzf' " Fuzzy search
Plug 'junegunn/fzf.vim' " Fuzzy search
Plug 'junegunn/vim-easy-align' " Alignment
Plug 'janko-m/vim-test' " Test running
Plug 'PeterRincker/vim-argumentative' " Function argument handling
Plug 'tpope/vim-abolish' " Abbreviation, Substitution, and Coercion
Plug 'tpope/vim-surround' " Surroundings
Plug 'tpope/vim-repeat' " Repeating plugin maps
Plug 'tpope/vim-dispatch' " Command running
Plug 'tpope/vim-unimpaired' " Complementary mappings
Plug 'scrooloose/nerdcommenter' " Commenting
Plug 'terryma/vim-multiple-cursors' " Multiple Cursors

" Completion, LSP, and snippets
if has("nvim")
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
endif

" Language Support

" Ruby and Rails
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
Plug 'alexbel/vim-rubygems'

" Javascript and related languages
Plug 'tpope/vim-jdaddy'
Plug 'pangloss/vim-javascript'
Plug 'elzr/vim-json'
Plug 'kchmck/vim-coffee-script'

" HTML & CSS
Plug 'mattn/emmet-vim'
Plug 'ap/vim-css-color'

" Elixir
Plug 'elixir-lang/vim-elixir'
Plug 'lucidstack/hex.vim'
Plug 'slashmili/alchemist.vim'

Plug 'ElmCast/elm-vim' " Elm
Plug 'fatih/vim-go' " Go
Plug 'tpope/vim-liquid' " Liquid
Plug 'tpope/vim-cucumber' " Cucumber

" Markdown
Plug 'tpope/vim-markdown'
if has("nvim")
  Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
endif
Plug 'mzlogin/vim-markdown-toc'

call plug#end()

"Built in plugins not loaded by default
runtime! plugin/matchit.vim
runtime! macros/matchit.vim

"}}}

" {{{ fugitive configuration
" Open a PR on GitHub based on the SHA while using :Gblame
" requires both vim-fugitive and vim-rhubarb
function! OpenPR(sha)
  let pr_number = system("git log --merges --ancestry-path --oneline ". a:sha . "..master | grep 'pull request' | tail -n1 | awk '{print $5}' | cut -c2-")
  let remote = fugitive#RemoteUrl(".")
  let root = rhubarb#homepage_for_url(remote)
  let url = root . '/pull/' . substitute(pr_number, '\v\C\n', '', 1)
  call netrw#BrowseX(url, 0)
endfunction

augroup fugitive_ext
  autocmd!
  " Browse to the commit under my cursor
  autocmd FileType fugitiveblame nnoremap <buffer> <localleader>gb :execute ":Gbrowse " . expand("<cword>")<cr>

  " Browse to the PR for commit under my cursor
  autocmd FileType fugitiveblame nnoremap <buffer> <localleader>pr :call OpenPR(expand("<cword>"))<cr>
augroup END

map ,gs :Gstatus<CR>
map ,gpl :Git pull<CR>
map ,gpr :Git pull --rebase<CR>
map ,gpu :Git push<CR>
map ,gdi :Git diff<CR>
map ,gdc :Git diff --cached<CR>
map ,ga :update \| Dispatch git add %<CR>

" }}}

" {{{ Airline configuration
let g:airline_powerline_fonts = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline_theme = "ayu"
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.branch = '⎇ '
let g:airline_symbols.paste = 'ρ'
let g:airline_mode_map = {
      \ '__' : '-',
      \ 'n'  : 'N',
      \ 'i'  : 'I',
      \ 'R'  : 'R',
      \ 'c'  : 'C',
      \ 'v'  : 'V',
      \ 'V'  : 'V',
      \ '' : 'V',
      \ 's'  : 'S',
      \ 'S'  : 'S',
      \ '' : 'S',
      \ }

" This is for if I add back coc
"let g:airline_section_error = '%{airline#util#wrap(airline#extensions#coc#get_error(),0)}'
"let g:airline_section_warning = '%{airline#util#wrap(airline#extensions#coc#get_warning(),0)}'
" }}}

" {{{ ag.vim configuration
if executable('ag')
    let g:ackprg = 'ag --vimgrep'
endif

if executable('rg')
    let g:ackprg = 'rg --vimgrep --color=never'
endif

" Ctrl-Shift-F for Ag
map <C-F> :Ack<Space>
" }}}

" {{{ vim-test configuration
nmap ,T :TestNearest<CR>
nmap ,t :TestFile<CR>
nmap ,a :TestSuite<CR>

let test#strategy = 'dispatch'
" }}}

" {{{ FZF configuration
let $FZF_DEFAULT_COMMAND='rg --files --hidden'
map ,f :Files<CR>
map ,gf :GFiles<CR>
" }}}

" {{{ Markdown preview configuration
let g:mkdp_command_for_global = 0
" }}}

" {{{ Ultisnips configuration
let g:UltiSnipsExpandTrigger="<C-Space>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
" }}}

" {{{ vim-multiple-cursor configuration
function! g:Multiple_cursors_before()
  call deoplete#custom#buffer_option('auto_complete', v:false)
endfunction

function! g:Multiple_cursors_after()
  call deoplete#custom#buffer_option('auto_complete', v:true)
endfunction
" }}} vim-multiple-cursor configuration

" {{{ vim-json configuration
let g:vim_json_syntax_conceal = 0
" }}}

" {{{ Vim configuration

" Use a shell that's always there
set shell=/bin/sh

"Color scheme
let ayucolor="dark" " other options are `light` or `mirage`
colorscheme hackthebox

set hidden "background buffers without writing them. saves marks/undo as well

set number " absolute line numbers
set ruler " line and column number of cursor
set laststatus=2 " always show the status line (ruler goes in the status line)
set report=0 " Always report number of lines changed by ex commands

"Short message options. Vim docs have the full list. My chosen options are
"below.
"flag	meaning when present
" f	use "(3 of 5)" instead of "(file 3 of 5)"
" i	use "[noeol]" instead of "[Incomplete last line]"
" l	use "999L, 888C" instead of "999 lines, 888 characters"
" m	use "[+]" instead of "[Modified]"
" n	use "[New]" instead of "[New File]"
" r	use "[RO]" instead of "[readonly]"
" w	use "[w]" instead of "written" for file write message
"               and "[a]" instead of "appended" for ':w >> file' command
" x	use "[dos]" instead of "[dos format]", "[unix]" instead of
"               "[unix format]" and "[mac]" instead of "[mac format]".
" t	truncate file message at the start if it is too long to fit
"               on the command-line, "<" will appear in the left most column.
"               Ignored in Ex mode.
" T	truncate other messages in the middle if they are too long to
"               fit on the command line.  "..." will appear in the middle.
"               Ignored in Ex mode.
" o	overwrite message for writing a file with subsequent message
"               for reading a file (useful for ":wn" or when 'autowrite' on)
" O	message for reading a file overwrites any previous message.
"               Also for quickfix message (e.g., ":cn").
set shortmess=filmnrwxtToO

" Don't wrap for display by default. Often overwritten by filetypes
set nowrap

" Don't beep
set visualbell

" Default indentation options.
" - Always use spaces
" - Two space indent
" - Tab at start of line always indents
" - Round indents to two spaces
set et sta sr ts=2 sts=2 sw=2

" Allow modelines to work and search for them in the first 10 and last 10
" lines of files
set modeline
set modelines=10


" Searches. Highlight them, be incremental, ignore case by default, pay
" attention to case when upper case letters are present
set hlsearch
set incsearch
set ignorecase
set smartcase

" Patterns to ignore when expanding wildcards
set wildignore+=.hg,.git,.svn
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.sw?
set wildignore+=*.DS_Store?
set wildignore+=vendor/bundle
set wildignore+=vendor/gems
set wildignore+=log/**
set wildignore+=node_modules/**

" Show partial commands in the command line
set showcmd

" {{{ Terminal configuration
" prevent Vim from clobbering the scrollback buffer
set t_ti= t_te=

" Set things up for true color
set termguicolors
if has("termguicolors") && &tgc && &t_8f == ''
  set t_8f=[38;2;%lu;%lu;%lum
  set t_8b=[48;2;%lu;%lu;%lum
endif

set timeoutlen=1200 "more time for typing complex mappings
set ttimeoutlen=10 "Make esc work faster
" }}}

" {{{ Swap file configuration
" I don't use them and want to keep vim's cruft contained
set backup
set noswapfile
set undodir=~/.vim/tmp/undo
set backupdir=~/.vim/tmp/backup
set directory=~/.vim/tmp/backup

if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif

if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
" }}}

" List mode configuration
" on by default, with my custom character setup based on how capable my
" terminal is

if ((&termencoding ==# 'utf-8' || &encoding ==# 'utf-8') && version >= 700) || has("gui_running")
  set listchars=trail:·,precedes:«,extends:»,tab:▸-
  set showbreak=↪
else
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<
  set showbreak='+++ '
endif

set list

" Window configuration. New windows on the bottom and the right and don't make
" them the same size unless I say so
set splitbelow splitright noequalalways

" Movement configuration
set nostartofline "Attempt to keep the cursor in the same column
set scrolloff=3 "3 lines of context when scrolling
set showmatch "show matching pairs
set backspace=indent,eol,start "backspace over everything!

" GUI options, for the rare times I use it
set guioptions-=T "no toolbar
set guioptions-=m "or menu
set guioptions-=r "or right hand scrollbar
set guioptions-=R
set guioptions-=l "or left hand scrollbar
set guioptions-=L
set guifont=Monospace\ 9

" }}}

" {{{ Generic custom mappings
" I use a hybrid 'dual leader' mapping scheme. <Leader> is the default `\` and
" `,` gets used as a defacto leader key. You'll see the scheme used throughout
" each plugin's configuration as well as filetype specific configuration.

"Nuke the help mapping
noremap K <Nop>

"Make Y consistent with D and C. Yanks until EOL
nnoremap Y y$

"Make myself use hjkl instead of arrow keys in normal mode
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

" Opens a write command with the path of the currently edited file filled in
map <Leader>w :w <C-R>=expand("%:p:h") . "/" <CR>

" Opens an edit command with the path of the currently edited file filled in
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Opens a tab edit command with the path of the currently edited file filled in
map <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

" Inserts the path of the currently edited file into a command
" Command mode: Ctrl+P
cmap <C-P> <C-R>=expand("%:p:h") . "/" <CR>

" Clear the search buffer when hitting return
nnoremap <CR> :nohlsearch<CR>

" <Leader>= to make all windows the same size
map <Leader>= <C-w>=

" <Leader><Leader> to edit an alternate file
nnoremap <Leader><Leader> <C-^>

"Remove trailing whitespace
nnoremap <silent> ,sw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" Mappings for numbering
map ,nr :set rnu!<CR>
map ,na :set nu!<CR>

" System clipboard niceties. (Stolen from Mastering Vim Quickly)
map ,y "+y
map ,d "+d
map ,p "+p
map ,P "+P

" CTags
map <Leader>rt :!ctags -R *<CR><CR>
map <C-\> :tnext<CR>

" Quickfix mappings
map ,cc :cclose<cr>
map ,cn :cnext<cr>
map ,cp :cprev<cr>

" Movement mappings
noremap H ^
noremap L $
noremap j gj
noremap k gk

" Make <leader>' switch between ' and "
nnoremap ,' ""yls<C-r>={'"': "'", "'": '"'}[@"]<CR><Esc>

" }}}

" {{{ Generic custom autocommands
" As much as possible, these are language independent
if has("autocmd")
  augroup generic_vim_fu
    autocmd!

    "Write all files when losing focus
    autocmd FocusLost * silent! wall

    "Update the git status when regaining focus
    autocmd FocusGained * silent! call fugitive#reload_status()

    "look more like less when using vim as less
    autocmd SourcePre */macros/less.vim set ls=0 cmdheight=1

    "Make new files writeable by default if they start with a sha-bang
    "This works regardless of filetype
    autocmd BufNewFile */.netrc,*/.fetchmailrc,*/.my.cnf let b:chmod_new="go-rwx"
    autocmd BufNewFile  * let b:chmod_exe=1
    autocmd BufWritePre * if exists("b:chmod_exe") |
          \ unlet b:chmod_exe |
          \ if getline(1) =~ '^#!' | let b:chmod_new="+x" | endif |
        \ endif
    autocmd BufWritePost,FileWritePost * if exists("b:chmod_new")|
          \ silent! execute "!chmod ".b:chmod_new." <afile>"|
          \ unlet b:chmod_new|
          \ endif

    "Unzip jars
    autocmd BufReadCmd *.jar call zip#Browse(expand("<amatch>"))

    " No line numbers for quickfix windows
    autocmd FileType qf   setl number norelativenumber

    " Jump to last cursor position unless it's invalid or in an event handler
    autocmd BufReadPost *
          \ if line("'\"") > 0 && line("'\"") <= line("$") |
          \   exe "normal g`\"" |
          \ endif

    " Make all windows equal size if vim is resized
    autocmd VimResized * exe "normal! \<C-w>="

  augroup END
endif
" }}}

" {{{ Custom commands

"Write out with sudo
command! -bar -nargs=0 SudoW   :setl nomod|silent exe 'write !sudo tee % >/dev/null'|let &mod = v:shell_error

"Use 'W' to write as well
command! -bar -nargs=* -bang W :write<bang> <args>

"Create a scratch file
command! -bar -nargs=0 -bang Scratch :silent edit<bang> \[Scratch]|set buftype=nofile bufhidden=hide noswapfile buflisted

" }}}

" {{{ Custom Functions
"Rename current file
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>
" }}}
" vim: set et sts=2 sw=2 ts=16 fdm=marker :
