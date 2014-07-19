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

let g:CommandTMaxHeight=20

let g:airline_powerline_fonts = 1
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
set shortmess+=a

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

set encoding=utf-8
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

" Comma-f for Command-T in normal mode
map ,f :CommandTFlush<CR>\|:CommandT<CR>
" Control-f for Command-T in insert mode
imap <C-f> <Esc>:CommandTFlush<CR>\|:CommandT<CR>

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

noremap H ^
noremap L $

noremap j gj
noremap k gk

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
map ,r :wa\|:!osascript ~/bin/chrome_reload_tab.scpt<CR><CR>

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

" GUI Specific commands {{{
command! -bar -nargs=0 Bigger  :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)+1','')
command! -bar -nargs=0 Smaller :let &guifont = substitute(&guifont,'\d\+$','\=submatch(0)-1','')
noremap <M-,>        :Smaller<CR>
noremap <M-.>        :Bigger<CR>

" }}}

" Custom Functions {{{

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

"Switch between test and production code
function! OpenTestAlternate()
  let new_file = AlternateForCurrentFile()
  exec ':e ' . new_file
endfunction

function! AlternateForCurrentFile()
  let current_file = expand("%")
  let new_file = current_file
  let in_spec = match(current_file, '^spec/') != -1 || match(current_file, '^spec_no_rails/') != -1
  let going_to_spec = !in_spec
  let in_app = match(current_file, '\<controllers\>') != -1 || match(current_file, '\<models\>') != -1 || match(current_file, '\<views\>') != -1 || match(current_file, '\<helpers\>') != -1 || match(current_file, '\<services\>') != -1
  if going_to_spec
    if in_app
      let new_file = substitute(new_file, '^app/', '', '')
    end
    let new_file = substitute(new_file, '\.rb$', '_spec.rb', '')
    let new_file = 'spec/' . new_file
  else
    let new_file = substitute(new_file, '_spec\.rb$', '.rb', '')
    let new_file = substitute(new_file, '^spec/', '', '')
    let new_file = substitute(new_file, '^spec_no_rails/', '', '')
    if in_app
      let new_file = 'app/' . new_file
    end
  endif
  return new_file
endfunction
nnoremap <leader>. :call OpenTestAlternate()<cr>

map ,t :call RunTestFile()<cr>
map ,T :call RunNearestTest()<cr>
map ,a :call RunTests('')<cr>
map ,c :w\|:!script/features<cr>
map ,w :w\|:!script/features --profile wip<cr>

function! RunTestFile(...)
    if a:0
        let command_suffix = a:1
    else
        let command_suffix = ""
    endif

    " Run the tests for the previously-marked file.
    let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.rb\)$') != -1
    if in_test_file
        call SetTestFile()
    elseif !exists("t:grb_test_file")
        return
    end
    call RunTests(t:grb_test_file . command_suffix)
endfunction

function! RunNearestTest()
    let spec_line_number = line('.')
    call RunTestFile(":" . spec_line_number . " -b")
endfunction

function! SetTestFile()
    " Set the spec file that tests will be run for.
    let t:grb_test_file=@%
endfunction

function! RunTests(filename)
    " Write the file and run tests for the given filename
    :w
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    :silent !echo;echo;echo;echo;echo;echo;echo;echo;echo;echo
    if match(a:filename, '\.feature$') != -1
        exec ":!script/features " . a:filename
    elseif match(a:filename, '_test\.rb$') != -1
        if filereadable("Gemfile")
            exec ":!bundle exec rake test TEST=" . a:filename
        else
            exec ":!rake test TEST=" . a:filename
        end
    else
        if filereadable("script/test")
            exec ":!script/test " . a:filename
        elseif filereadable("Gemfile")
            exec ":!bundle exec rspec --color " . a:filename
        else
            exec ":!rspec --color " . a:filename
        end
    end
endfunction

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
