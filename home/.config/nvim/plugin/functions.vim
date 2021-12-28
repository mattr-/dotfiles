" Emulate ack.vim (when used with dispatch)
function! Grep(cmd, args)
  redraw
  " If nothing provided, use the word under the cursor. If still nothing,
  " provide a nice error message
  let l:grepargs = empty(a:args) ? expand("<cword>") : a:args
  if l:grepargs == ""
    echo "Nothing to search for."
    return
  endif

  echo "Searching. Please wait..."
  silent execute a:cmd l:grepargs
  execute 'botright copen'
  echo ""
  redraw!
endfunction

command! -bang -nargs=* -complete=file Grep call Grep('grep<bang>', <q-args>)

nmap <c-f> :Grep 
