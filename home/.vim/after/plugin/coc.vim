if has("nvim")
  " Settings for coc
  " https://github.com/neoclide/coc.nvim#example-vim-configuration
  inoremap <silent><expr> <c-space> coc#refresh()

  " gd - go to definition of word under cursor
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)

  " gi - go to implementation
  nmap <silent> gi <Plug>(coc-implementation)

  " gr - find references
  nmap <silent> gr <Plug>(coc-references)

  " gh - get hint on whatever's under the cursor
  nnoremap <silent> K :call <SID>show_documentation()<CR>
  nnoremap <silent> gh :call <SID>show_documentation()<CR>

  function! s:show_documentation()
    if &filetype == 'vim'
      execute 'h '.expand('<cword>')
    else
      call CocAction('doHover')
    endif
  endfunction


  " Highlight symbol under cursor on CursorHold
  autocmd CursorHold * silent call CocActionAsync('highlight')

  nnoremap <silent> ,co  :<C-u>CocList outline<cr>
  nnoremap <silent> ,cs  :<C-u>CocList -I symbols<cr>

  " List errors
  nnoremap <silent> ,cl  :<C-u>CocList locationlist<cr>

  " list commands available in tsserver (and others)
  "nnoremap <silent> ,cc  :<C-u>CocList commands<cr>

  " restart when tsserver gets wonky
  nnoremap <silent> ,cR  :<C-u>CocRestart<CR>

  " view all errors
  nnoremap <silent> ,cl  :<C-u>CocList locationlist<CR>

  " manage extensions
  nnoremap <silent> ,cx  :<C-u>CocList extensions<cr>

  " rename the current word in the cursor
  nmap ,cr  <Plug>(coc-rename)
  nmap ,cf  <Plug>(coc-format-selected)
  vmap ,cf  <Plug>(coc-format-selected)

  " run code actions
  vmap ,ca  <Plug>(coc-codeaction-selected)
  nmap ,ca  <Plug>(coc-codeaction-selected)
endif
