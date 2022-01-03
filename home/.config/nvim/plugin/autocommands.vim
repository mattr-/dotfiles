augroup generic_vimrc
  autocmd!

  "Write all files when losing focus
  autocmd FocusLost * silent! wall

  "Update the git status when regaining focus.
  " TODO Remove this once we decide to ditch fugitive for good
  " autocmd FocusGained * silent! call fugitive#reload_status()

  "look more like less when using vim as less
  autocmd SourcePre */macros/less.vim set ls=0 cmdheight=1

  "Make new files executable by default if they start with a sha-bang
  "This works regardless of filetype
  autocmd BufNewFile * let b:chmod_exe=1
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

  " No line numbers for quickfix windows
  autocmd FileType qf   setl number norelativenumber

  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif

  " Make all windows equal size if vim is resized
  autocmd VimResized * exe "normal! \<C-w>="

augroup END
