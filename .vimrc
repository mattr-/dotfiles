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

"Settings {{{

set background=dark
color evening

if has("gui_running")
    set background=light
    color solarized
endif

set hidden "background buffers without writing them. saves marks/undo as well

set number
set ruler
set laststatus=2
set report=0
set shortmess+=a

set nowrap
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set smarttab

set hlsearch
set incsearch
set ignorecase
set smartcase

set wildmenu "make tab completion behave like bash

set encoding=utf-8
set showcmd "show partial command in the status line

" swap files
set backupdir=~/.vim/backup
set directory=~/.vim/backup

" Modelines
set modeline
set modelines=10

set lazyredraw
set visualbell "dont beep

if (&termencoding ==# 'utf-8' || &encoding ==# 'utf-8') && version >= 700 && has("gui_running")
  let &listchars = "tab:\u21e5\u00b7,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u26ad"
else
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<
endif

set nostartofline
set scrolloff=3 "3 lines of context when scrolling
set showmatch "show matching pairs
set backspace=indent,eol,start "backspace over everything!

set splitbelow "new windows on the bottom
set noequalalways

set timeoutlen=1200 "more time for macros
set ttimeoutlen=50 "Make esc work faster


set guioptions-=T "no toolbar
set guioptions-=m "or menu
set guioptions-=r "or right hand scrollbar
set guioptions-=R 
set guioptions-=l "or left hand scrollbar
set guioptions-=L
set guifont=Monospace\ 9

if has("gui_running")
    set cursorline
endif

" }}}

"Mappings {{{

"Make F1 emulate Escape - taken from the Hashrocket dotfiles
noremap <F1> <Esc>
noremap! <F1> <Esc>

"Make Ctrl-C emulate Escape
inoremap <C-c> <Esc>

"Nuke the help mapping
noremap K <Nop>

"Make Y consistent with D and C
nnoremap Y y$

"Make myself use hjkl instead of arrow keys
map <Left> :echo "no! use \'h\' instead!"<cr>
map <Right> :echo "no! use \'l\' instead!"<cr>
map <Up> :echo "no! use \'k\' instead!"<cr>
map <Down> :echo "no! use \'j\' instead!"<cr>

" Opens an edit command with the path of the currently edited file filled in
" Normal mode: <Leader>e
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Opens a tab edit command with the path of the currently edited file filled in
" Normal mode: <Leader>te
map <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

" Inserts the path of the currently edited file into a command
" Command mode: Ctrl+P
cmap <C-P> <C-R>=expand("%:p:h") . "/" <CR>

" Clear the search buffer when hitting return
nnoremap <CR> :nohlsearch<CR>

" Comma-f for Command-T in normal mode
map ,f :CommandT<CR>

" Ctrl-Shift-F for Ack
map <C-F> :Ack<Space>

" <Leader>= to make all windows the same size
map <Leader>= <C-w>=

" <Leader><Leader> to edit an alternate file
nnoremap <Leader><Leader> <C-^>

" Hashrocket with <C-l>
imap <C-l> <space>=><space>
"}}}

"Command-T configuration
let g:CommandTMaxHeight=20

" CTags
map <Leader>rt :!ctags --c++-kinds=+pl --fields=+iaS --extra=+f+q --languages=-javascript,-sql -R *<CR><CR>
map <C-\> :tnext<CR>

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
        autocmd FileType sh,zsh,csh,tcsh,perl,python,ruby imap <buffer> <C-X>& <C-X>!<Esc>o <C-U># $I<C-V>d$<Esc>o <C-U><C-X>^<Esc>o <C-U><C-G>
        autocmd FileType c,cpp,cs,java,perl,javascript,css let b:surround_101 = "\r\n}"
        autocmd FileType apache                 setlocal commentstring=#\ %s
        autocmd FileType css silent! setlocal omnifunc=csscomplete#CompleteCSS
        autocmd FileType cucumber silent! compiler cucumber | setl makeprg=cucumber\ \"%:p\" | imap <buffer><expr> <Tab> pumvisible() ? "\<C-N>" : (CucumberComplete(1,'') >= 0 ? "\<C-X>\<C-O>" : (getline('.') =~ '\S' ? ' ' : "\<C-I>"))
        autocmd FileType git,gitcommit setlocal foldmethod=syntax foldlevel=1
        autocmd FileType make setl noexpandtab
         
    augroup END " }}}2
    "Makefiles

    "Text
    au FileType text setl ai tw=78


    "Cucumber
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

" GUI Specific commands {{{
command! -bar -nargs=0 Bigger  :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)+1','')
command! -bar -nargs=0 Smaller :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)-1','')
noremap <M-,>        :Smaller<CR>
noremap <M-.>        :Bigger<CR>

" }}}

" Custom Functions {{{

function! RunLineAsCommand()
    let current_line = getline(line('.'))
    exec ":!" . current_line
endfunction

map ,rc :call RunLineAsCommand
" }}}


" vim: set et sts=4 sw=4 ts=16 fdm=marker :
