vim.opt.termguicolors = true
require("mattr-.theme").set()
local status_ok, colorizer = pcall(require, "colorizer")
if status_ok then
  colorizer.setup()
end
