scriptencoding utf-8

" I use a hybrid 'dual leader' mapping scheme. <Leader> is the default `\` and
" `,` gets used as a defacto leader key. You'll see the scheme used throughout
" each plugin's configuration as well as filetype specific configuration.

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
nmap <Leader>w :w <C-R>=expand("%:p:h") . "/" <CR>

" Opens an edit command with the path of the currently edited file filled in
nmap <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Opens a tab edit command with the path of the currently edited file filled in
nmap <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

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

" vim: set et sts=2 sw=2 ts=16 fdm=marker :
