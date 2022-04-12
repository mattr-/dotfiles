-- Store startup time in seconds
vim.g.start_time = vim.fn.reltime()

vim.g.gitblame_date_format = "%r"

require "mattr-.globals"
require "mattr-.plugins"
require "mattr-.lsp"

-- Telescope configuration
require "mattr-.telescope.setup"
require "mattr-.telescope.mappings"
