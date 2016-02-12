set nocompatible

"load pathogen from the bundle
runtime bundle/vim-pathogen/autoload/pathogen.vim

"Set up pathogen and the bundle loading
filetype off
call pathogen#infect()
syntax on
filetype plugin indent on

"Extra plugins
runtime! plugin/matchit.vim
runtime! macros/matchit.vim

"Plugin settings {{{
let g:ragtag_global_maps = 1

let g:CommandTMaxHeight=10

let g:airline_powerline_fonts = 1
let g:airline#extensions#syntastic#enabled = 1

let g:ctrlp_map = ',f'
let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
      \ --ignore .git
      \ --ignore .DS_Store
      \ --ignore .bundle
      \ -g ""'
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
let g:ctrlp_use_caching = 0

let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_aggregate_errors = 1
let g:syntastic_enable_signs = 1
let g:syntastic_error_symbol = "⛔️ "
let g:syntastic_warning_symbol = "⚠️ "

if has("nvim")
    set mouse-=a
    let test#strategy = "neoterm"
    "Escape for normal mode in terminal mode
    tnoremap <Esc> <C-\><C-n>
endif
" }}}

"Settings {{{
set shell=/bin/sh

set background=light
color mattr

set hidden "background buffers without writing them. saves marks/undo as well

set number
set ruler
set laststatus=2
set report=0
set shortmess=filmnrwxtToO

set nowrap
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set smarttab
set shiftround

set hlsearch
set incsearch
set ignorecase
set smartcase
set winwidth=78

set wildmenu "make tab completion behave like bash
set wildmode=list:longest

set wildignore+=.hg,.git,.svn
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.sw?
set wildignore+=*.DS_Store?
set wildignore+=vendor/bundle
set wildignore+=vendor/gems
set wildignore+=log/**
set wildignore+=node_modules/**

set showcmd "show partial command in the status line

" prevent Vim from clobbering the scrollback buffer
set t_ti= t_te=

" Use 256 colors all the time
set t_Co=256

" swap files
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

" Modelines
set modeline
set modelines=10

set visualbell "dont beep

if ((&termencoding ==# 'utf-8' || &encoding ==# 'utf-8') && version >= 700) || has("gui_running")
  set listchars=trail:·,precedes:«,extends:»,tab:▸-
  set showbreak=↪
else
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<
  set showbreak='+++ '
endif

set list
set nostartofline
set scrolloff=3 "3 lines of context when scrolling
set showmatch "show matching pairs
set backspace=indent,eol,start "backspace over everything!

set splitbelow "new windows on the bottom
set splitright "and on the right
set noequalalways

set timeoutlen=1200 "more time for macros
set ttimeoutlen=10 "Make esc work faster


set guioptions-=T "no toolbar
set guioptions-=m "or menu
set guioptions-=r "or right hand scrollbar
set guioptions-=R
set guioptions-=l "or left hand scrollbar
set guioptions-=L
set guifont=Monospace\ 9

set cursorline

" }}}

"Mappings {{{

"Make kj emulate Escape
imap kj <Esc>

"Make myself use Ctrl-H and Ctrl-W
inoremap <C-K> <C-O>D

"Nuke the help mapping
noremap K <Nop>

"Make Y consistent with D and C
nnoremap Y y$

"Make myself use hjkl instead of arrow keys
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

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

" Ctrl-Shift-F for Ag
map <C-F> :Ag<Space>

" <Leader>= to make all windows the same size
map <Leader>= <C-w>=

" <Leader><Leader> to edit an alternate file
nnoremap <Leader><Leader> <C-^>

" Hashrocket with <C-l>
imap <C-l> <space>=><space>

" Make <leader>' switch between ' and "
nnoremap ,' ""yls<C-r>={'"': "'", "'": '"'}[@"]<CR><Esc>

" Fugitive/Git mappings
map ,gs :Gstatus<CR>
map ,gpl :Git pull<CR>
map ,gpr :Git pull --rebase<CR>
map ,gpu :Git push<CR>
map ,gdi :Git diff<CR>
map ,gdc :Git diff --cached<CR>
map ,ga :update \| Git add %<CR>

" vim-test mappings
nmap ,T :TestNearest<CR>
nmap ,t :TestFile<CR>
nmap ,a :TestSuite<CR>

" My custom test mappings
map ,ca :w\|:!script/features<cr>
map ,cw :w\|:!script/features --profile wip<cr>

noremap H ^
noremap L $

noremap j gj
noremap k gk

" Use the alternate command
nnoremap <leader>. :A<CR>

" CTags
map <Leader>rt :!ctags --c++-kinds=+pl --fields=+iaS --extra=+f+q --languages=-javascript,-sql -R *<CR><CR>
map <C-\> :tnext<CR>

"Close tag
imap ,/ </<C-X><C-O>

"Remove trailing whitespace
nnoremap <silent> ,sw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" Mappings for numbering
map ,nr :set rnu!<CR>
map ,na :set nu!<CR>

" Save a file and reload the active tab in Chrome
map ,r :!osascript ~/bin/chrome_reload_tab.scpt<CR><CR>

" Turn off the ActiveRecord rails mapping
map ,mar :Rabbrev! AR<CR>
map ,ear :Rabbrev AR:: ActiveRecord<CR>

" Restart a rails server
map ,br :execute 'silent !tmux send-keys -t server C-c rs C-m'<Bar>redraw!<CR>

" Switch hash keys with values
map ,ks :s/\([:_a-zA-z]\+\) => \([a-zA-Z:_]\+\)/\2 => \1/g<CR>

" Migrate and rollback
map ,dbm :!bin/rake db:migrate<CR>
map ,dbr :!bin/rake db:rollback<CR>
"}}}

" File type setup for files unknown to Vim {{{
if has("autocmd")
    "File types and stuff
    au BufRead,BufNewFile {GemFile,Rakefile,VagrantFile,Thorfile,config.ru} set ft=ruby
    au BufRead,BufNewFile *.thor set ft=ruby
    au BufRead,BufNewFile *.god set ft=ruby
    au BufRead,BufNewFile *.json set ft=javascript
    au BufRead,BufNewFile *.jasmine_fixture set ft=html
endif
"}}}

" File type configuration for known filetypes {{{
if has("autocmd")
    augroup FTMisc " {{{2
        autocmd!

        "Write all files when losing focus
        autocmd FocusLost * silent! wall

        "Update the git status when regaining focus
        autocmd FocusGained * silent! call fugitive#reload_status()

        "look more like less when using vim as less
        autocmd SourcePre */macros/less.vim set ls=0 cmdheight=1

        "Make new files writeable by default if they start with a sha-bang
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

    augroup END "}}}2

    augroup FTCheck " {{{2
        autocmd!
        autocmd BufNewFile,BufRead /etc/udev/rules.d/*.rules set ft=udev
        autocmd BufNewFile,BufRead *.txt,README,INSTALL,NEWS,TODO if &ft == ""|set ft=text|endif
        autocmd BufRead * if ! did_filetype() && getline(1)." ".getline(2).
                  \ " ".getline(3) =~? '<\%(!DOCTYPE \)\=html\>' | setf html | endif
        autocmd BufNewFile,BufRead html.erb set ft=eruby.html
        autocmd BufNewFile,BufRead js.erb set ft=eruby.javascript
        autocmd BufNewFile,BufRead css.erb set ft=eruby.css
        autocmd BufNewFile,BufRead scss.erb set ft=eruby.scss
        autocmd BufNewFile,BufRead less.erb set ft=eruby.less

    augroup END " }}}2

    augroup FTOptions " {{{2
        autocmd!
        autocmd FileType c,cpp,cs,java          setlocal ai et sta sw=4 sts=4 cin
        autocmd FileType sh,csh,tcsh,zsh        setlocal ai et sta sw=4 sts=4
        autocmd FileType tcl,perl,python        setlocal ai et sta sw=4 sts=4
        autocmd FileType markdown,liquid        setlocal ai et sta sw=2 sts=2 tw=72
        autocmd FileType javascript             setlocal ai et sta sw=2 sts=2 ts=2 cin isk+=$
        autocmd FileType css,scss               setlocal ai et sta sw=2 sts=2
        autocmd FileType html,xhtml             setlocal ai et sta sw=2 sts=2
        autocmd FileType eruby,yaml,ruby        setlocal ai et sta sw=2 sts=2
        autocmd FileType cucumber               setlocal ai et sta sw=2 sts=2 ts=2
        autocmd FileType sh,zsh,csh,tcsh        inoremap <silent> <buffer> <C-X>! #!/bin/<C-R>=&ft<CR>
        autocmd FileType perl,python,ruby       inoremap <silent> <buffer> <C-X>! #!/usr/bin/<C-R>=&ft<CR>
        autocmd FileType c,cpp,cs,java,perl,javascript,css let b:surround_101 = "\r\n}"
        autocmd FileType apache                 setlocal commentstring=#\ %s
        autocmd FileType css silent! setlocal omnifunc=csscomplete#CompleteCSS
        autocmd FileType cucumber silent! compiler cucumber | setl makeprg=cucumber\ \"%:p\" | imap <buffer><expr> <Tab> pumvisible() ? "\<C-N>" : (CucumberComplete(1,'') >= 0 ? "\<C-X>\<C-O>" : (getline('.') =~ '\S' ? ' ' : "\<C-I>"))
        autocmd FileType git,gitcommit setlocal foldmethod=syntax foldlevel=1
        autocmd FileType make setl noexpandtab
        autocmd FileType text setl ai tw=78
    augroup END " }}}2
endif

" }}}

" Custom commands and their mappings {{{

"Write out with sudo
command! -bar -nargs=0 SudoW   :setl nomod|silent exe 'write !sudo tee % >/dev/null'|let &mod = v:shell_error

"Use 'W' to write as well
command! -bar -nargs=* -bang W :write<bang> <args>

"Create a scratch file
command! -bar -nargs=0 -bang Scratch :silent edit<bang> \[Scratch]|set buftype=nofile bufhidden=hide noswapfile buflisted

"Load an RFC in vim
command! -bar -count=0 RFC     :e http://www.ietf.org/rfc/rfc<count>.txt|setl ro noma

"Rename a file
command! -bar -nargs=* -bang -complete=file Rename :
      \ let v:errmsg = ""|
      \ saveas<bang> <args>|
      \ if v:errmsg == ""|
      \   call delete(expand("#"))|
      \ endif

" }}}

" Custom Functions and their mappings {{{

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

"Promote variable to rspec let
function! PromoteToLet()
  :s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
  :silent normal ==
endfunction
command! PromoteToLet :call PromoteToLet()
map <leader>p :PromoteToLet<cr>

" }}}

" Misc AutoCommands {{{
augroup vimrcEx
  " Clear all autocmds in the group
  autocmd!
  autocmd FileType text setlocal textwidth=78
  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
  autocmd VimResized * exe "normal! \<C-w>="
" }}}


" vim: set et sts=4 sw=4 ts=16 fdm=marker :
