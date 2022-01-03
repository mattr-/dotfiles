" For terminals, don't display line numbers
augroup terminal_customizations
  autocmd!
  autocmd BufWinEnter * if &buftype == 'terminal' | setlocal nonumber | endif
augroup END

