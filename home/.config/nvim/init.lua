-- Store startup time in seconds
vim.g.start_time = vim.fn.reltime()

require "mattr-.globals"
require "mattr-.plugins"
require "mattr-.lsp"

vim.defer_fn(function()
  require "mattr-.keybindings"

  vim.cmd([[
      PackerLoad which-key.nvim
  ]])
end, 0)
