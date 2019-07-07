" Hashrocket with <C-l>
imap <C-l> <space>=><space>

" Turn off the ActiveRecord rails mapping
map ,mar :Rabbrev! AR<CR>
map ,ear :Rabbrev AR:: ActiveRecord<CR>

" Switch hash keys with values
map ,ks :s/\([:_a-zA-z]\+\) => \([a-zA-Z:_]\+\)/\2 => \1/g<CR>

" Migrate and rollback
map ,dbm :!bin/rake db:migrate<CR>
map ,dbr :!bin/rake db:rollback<CR>

