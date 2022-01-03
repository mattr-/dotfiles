" For terminals, don't display line numbers
augroup terminal_customizations
  autocmd!
  autocmd TermOpen * setl nonumber norelativenumber
augroup END

