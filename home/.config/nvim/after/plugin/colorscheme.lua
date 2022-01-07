vim.opt.termguicolors = true
require("mattr-.theme").set()
require("mattr-.statusline")
local status_ok, colorizer = pcall(require, "colorizer")
if status_ok then
  colorizer.setup()
end
