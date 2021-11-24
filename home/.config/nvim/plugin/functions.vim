" Emulate ack.vim since I used so little of it's functionality
function! Grep(cmd, args)
  echo "Searching..."
  silent execute 'grep' a:args
  execute 'botright copen'
  redraw!
endfunction

command! -bang -nargs=* -complete=file Grep call Grep('grep<bang>', <q-args>)

nmap <c-f> :Grep 
