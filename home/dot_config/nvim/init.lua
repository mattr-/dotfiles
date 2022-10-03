-- Store startup time in seconds
vim.g.start_time = vim.fn.reltime()

require "mattr-.globals"
require "mattr-.plugins"
require "mattr-.lsp"

vim.defer_fn(function()
  if (packer_plugins and packer_plugins["which-key.nvim"])
  then
    vim.cmd([[
      PackerLoad which-key.nvim
    ]])
  end
end, 0)
