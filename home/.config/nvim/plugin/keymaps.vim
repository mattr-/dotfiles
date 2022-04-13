scriptencoding utf-8

" I use a hybrid 'dual leader' mapping scheme. <Leader> is the default `\` and
" `,` gets used as a defacto leader key. You'll see the scheme used throughout
" each plugin's configuration as well as filetype specific configuration.
" As of 2021-12-30, I'm changing things up and the use of `\` is going to be
" explicit

"Nuke the help mapping
noremap K <Nop>

"Make Y consistent with D and C. Yanks until EOL
nnoremap Y y$

"Make myself use hjkl instead of arrow keys in normal mode
nmap <Left> <Nop>
nmap <Right> <Nop>
nmap <Up> <Nop>
nmap <Down> <Nop>

" Opens a write command with the path of the currently edited file filled in
nmap \w :w <C-R>=expand("%:p:h") . "/" <CR>

" Opens an edit command with the path of the currently edited file filled in
nmap \e :e <C-R>=expand("%:p:h") . "/" <CR>

" Opens a tab edit command with the path of the currently edited file filled in
nmap \te :tabe <C-R>=expand("%:p:h") . "/" <CR>

" Inserts the path of the currently edited file into a command
" Command mode: Ctrl+P
cmap <C-P> <C-R>=expand("%:p:h") . "/" <CR>

" \\ to edit an alternate file
nnoremap \\ <C-^>

"Remove trailing whitespace
nnoremap <silent> ,sw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" CTags
map \rt :!ctags -R *<CR><CR>
map <C-\> :tnext<CR>

" Quickfix mappings
nnoremap ,cc :echom "--> ,qfc <--"<CR>
nnoremap ,qfc :cclose<CR>
nnoremap ,cn :echom "--> ,qfc <--"<CR>
nnoremap ,qfn :cnext<CR>
nnoremap ,cp :echom "--> ,qfp <--"<CR>
nnoremap ,qfp :cprev<CR>

" Movement mappings
noremap H ^
noremap L $
noremap j gj
noremap k gk

" Make <leader>' switch between ' and "
nnoremap ,' ""yls<C-r>={'"': "'", "'": '"'}[@"]<CR><Esc>

" Make Ctrl-a and Ctrl-e work like the default readline bindings
cnoremap <c-a> <Home>
cnoremap <c-e> <End>

" vim: set et sts=2 sw=2 ts=16 fdm=marker :
