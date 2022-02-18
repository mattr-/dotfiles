-- Impatient speeds up loading but we don't want to bail if it's not present
pcall(require, "impatient")

vim.g.gitblame_date_format = "%r"

if require "mattr-.first_load"() then
  return
end

require "mattr-.globals"
require "mattr-.plugins"
require "mattr-.lsp"

-- Telescope configuration
require "mattr-.telescope.setup"
require "mattr-.telescope.mappings"
