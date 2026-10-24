local snippet_trigger = function(key)
  local ls = require("luasnip")
  return function()
    local snippets = ls.get_snippets(vim.bo.filetype)
    for _, snippet in ipairs(snippets) do
      if snippet.name == key then
        ls.snip_expand(snippet)
        return
      end
    end
  end
end

vim.keymap.set('i', '<C-l>', snippet_trigger("hashrocket"), { buffer = true })
