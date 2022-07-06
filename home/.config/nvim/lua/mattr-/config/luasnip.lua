return function()
  local ls = require("luasnip")
  local types = require('luasnip.util.types')

  -- See lua/luasnip/config.lua for the defaults should they need to be change
  -- For now, change nothing
  ls.config.setup({
    ext_opts = {
      [types.choiceNode] = {
        active = { virt_text = {{ "●", "HacktheboxOrange"}} }
      },
    }
  })

  -- TODO: Attempt to load snippet files dynmaically from ~/.config/nvim/lua/mattr-/snippets/<filetype>.lua
  -- But for now, just hardcode the list.

  ls.add_snippets('markdown',R("mattr-.snippets.markdown"))
  ls.add_snippets('ruby', R("mattr-.snippets.ruby"))

  -- cmp is nice but luasnip still needs its own insert mode mappings
  vim.cmd [[
  imap <silent> <expr> <C-k> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<C-k>'
  imap <silent> <expr> <C-j> luasnip#jumpable(-1) ? '<Plug>luasnip-jump-prev' : '<C-j>'
  imap <silent> <expr> <C-l> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-l>'

  snoremap <silent> <c-k> <cmd>lua require('luasnip').jump(1)<CR>
  snoremap <silent> <c-j> <cmd>lua require('luasnip').jump(-1)<CR>
  snoremap <silent> <c-l> <cmd>lua require('luasnip').change_choice(1)<CR>
]]
end
